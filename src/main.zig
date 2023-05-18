const std = @import("std");

const fs = std.fs;
const os = std.os;
const io = std.io;
const mem = std.mem;
const ascii = std.ascii;

const ArrayList = std.ArrayList;

const GRAM_QUIT_TIMES = 3;
const GRAM_QUERY_LEN = 256;
const GRAM_VERSION = "0.1";

const SEPARATORS = " ,.()+-/*=~%[];";

const KEYWORDS: [46][]const u8 = [46][]const u8{
    "align",     "allowzero",   "and",            "asm",
    "async",     "await",       "break",          "callconv",
    "catch",     "comptime",    "const",          "continue",
    "defer",     "else",        "enum",           "errdefer",
    "error",     "export",      "extern",         "fn",
    "for",       "if",          "inline",         "noalias",
    "nosuspend", "noinline",    "opaque",         "or",
    "orelse",    "packed",      "pub",            "resume",
    "return",    "linksection", "struct",         "suspend",
    "switch",    "test",        "threadlocal",    "try",
    "union",     "unreachable", "usingnamespace", "var",
    "volatile",  "while",
};

const Row = struct {
    src: []u8,
    render: []u8,
    hl: []Highlight,
};

const Highlight = enum(u8) {
    number = 31,
    match = 34,
    string = 35,
    comment = 36,
    normal = 37,
};

const Key = enum(u8) {
    ctrl_c = 3,
    ctrl_f = 6,
    ctrl_h = 8,
    tab = 9,
    ctrl_l = 12,
    enter = 13,
    ctrl_q = 17,
    ctrl_s = 19,
    ctrl_u = 21,
    esc = 27,
    backspace = 127,
    arrow_left = 128,
    arrow_right,
    arrow_up,
    arrow_down,
    del,
    home,
    end,
    page_up,
    page_down,
    _,
};

const FindDirection = enum { Next, Previous, None };

const Syntax = enum { zig };

fn isSeparator(c: u8) bool {
    for (SEPARATORS) |s| if (s == c) return true;
    return false;
}

const Editor = struct {
    const Self = @This();

    allocator: mem.Allocator,
    file_path: []const u8,
    rows: ArrayList(Row),
    c: [1]u8,
    dirty: bool = false,
    quit_times: u3 = GRAM_QUIT_TIMES,
    raw_mode: bool = false,
    syntax: ?Syntax = Syntax.zig,
    orig_termios: os.termios = undefined,
    cx: usize = 0,
    cy: usize = 0,
    row_offset: usize = 0,
    col_offset: usize = 0,
    screenrows: u16 = 0,
    screencols: u16 = 0,
    status_message: ArrayList(u8),

    fn init(allocator: mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
            .file_path = undefined,
            .c = [1]u8{0},
            .rows = ArrayList(Row).init(allocator),
            .status_message = try ArrayList(u8).initCapacity(allocator, 80),
        };
    }

    fn getWindowSize(self: *Self) !void {
        var wsz: os.linux.winsize = undefined;
        const fd = @bitCast(usize, @as(isize, os.linux.STDOUT_FILENO));
        if (os.linux.syscall3(.ioctl, fd, os.linux.T.IOCGWINSZ, @ptrToInt(&wsz)) == -1 or wsz.ws_col == 0) {
            _ = try os.write(os.linux.STDOUT_FILENO, "\x1b[999C\x1b[999B");
            return self.getCursorPosition();
        } else {
            self.screenrows = wsz.ws_row;
            self.screencols = wsz.ws_col;
        }
    }

    fn updateWindowSize(self: *Self) !void {
        try self.getWindowSize();
        self.screenrows -= 2;
    }

    fn findRestoreHighlight(
        self: *Self,
        saved_hl: *?[]Highlight,
        saved_hl_ix: ?usize,
    ) void {
        if (saved_hl.*) |hl| {
            mem.copy(Highlight, self.rows.items[saved_hl_ix.?].hl, hl);
            saved_hl.* = null;
        }
    }

    fn updateSyntax(self: *Self, row: *Row) !void {
        row.hl = try self.allocator.realloc(row.hl, row.render.len);
        @memset(row.hl, Highlight.normal);
        var prev_sep: bool = true; // Tell the parser if 'i' points to start of word. */
        var found_quotes: bool = false;

        if (self.syntax == null) return;

        for (0..row.render.len) |i| {
            if (prev_sep and i != row.render.len - 1 and row.render[i] == '/' and row.render[i + 1] == '/') {
                @memset(row.hl, Highlight.comment);
                return;
            }

            if (found_quotes) {
                row.hl[i] = Highlight.string;
                if (row.render[i] == '"') found_quotes = false;
            } else {
                if (row.render[i] == '"') {
                    found_quotes = true;
                    row.hl[i] = Highlight.string;
                    prev_sep = false;
                    continue;
                }
            }

            if (!ascii.isPrint(row.render[i])) {
                row.hl[i] = Highlight.normal;
                prev_sep = false;
                continue;
            }

            if (prev_sep) {
                keyword_match: for (KEYWORDS) |keyword| {
                    if (std.mem.eql(
                        u8,
                        keyword,
                        row.render[i..std.math.min(row.render.len - 1, i + keyword.len)],
                    ) and isSeparator(row.render[i + keyword.len])) {
                        prev_sep = false;
                        @memset(row.hl[i..std.math.min(row.render.len - 1, i + keyword.len)], Highlight.number);
                        break :keyword_match;
                    }
                }

                prev_sep = false;
                continue;
            }
            prev_sep = isSeparator(row.render[i]);
        }

        return;
    }

    fn find(self: *Self) !void {
        var query: [GRAM_QUERY_LEN]u8 = mem.zeroes([GRAM_QUERY_LEN]u8);
        var qlen: usize = 0;
        var last_match: ?usize = null; // Last line where a match was found. null for none.
        var find_direction: FindDirection = .None;
        var saved_hl: ?[]Highlight = null;
        var saved_hl_ix: ?usize = null;

        var saved_cx = self.cx;
        var saved_cy = self.cy;
        var saved_col_offset = self.col_offset;
        var saved_row_offset = self.row_offset;

        while (true) {
            // 49 is the amount of characters allowed for query, since our status message is capped at 80.
            try self.setStatusMessage("Search: {s} (Use ESC/Arrows/Enter)", .{query[0..std.math.min(qlen, 49)]});
            try self.refreshScreen();

            var c = try self.readKey();
            switch (@intToEnum(Key, c)) {
                .del, .ctrl_h, .backspace => {
                    if (qlen != 0) {
                        qlen -= 1;
                    }
                    last_match = null;
                },
                .enter, .esc => {
                    if (@intToEnum(Key, c) == .esc) {
                        self.cx = saved_cx;
                        self.cy = saved_cy;
                        self.col_offset = saved_col_offset;
                        self.row_offset = saved_row_offset;
                    }

                    self.findRestoreHighlight(&saved_hl, saved_hl_ix);
                    try self.setStatusMessage("", .{});
                    return;
                },
                .arrow_up, .arrow_left => find_direction = .Previous,
                .arrow_down, .arrow_right => find_direction = .Next,
                else => {
                    if (ascii.isPrint(c)) {
                        query[qlen] = c;
                        qlen += 1;
                        last_match = null;
                    }
                },
            }

            // Search occurrence.
            if (last_match == null) find_direction = .Next;
            if (find_direction == .Next or find_direction == .Previous) {
                var match: ?usize = null;
                var current: usize = if (last_match) |safe_last_match| safe_last_match else 0;

                match_for: for (self.rows.items) |_| {
                    switch (find_direction) {
                        FindDirection.Next => current += 1,
                        FindDirection.Previous => current = if (current - 1 == 0) 0 else current - 1,
                        else => {},
                    }

                    if (find_direction == .Previous and current == 0) current = self.rows.items.len - 1;
                    if (current == self.rows.items.len) current = 0;

                    match = mem.indexOf(u8, self.rows.items[current].render, query[0..qlen]);
                    if (match) |_| {
                        break :match_for;
                    }
                }

                find_direction = .None;
                self.findRestoreHighlight(&saved_hl, saved_hl_ix);

                if (match) |safe_match| {
                    var row = self.rows.items[current];
                    last_match = current;

                    saved_hl_ix = current;
                    saved_hl = try self.allocator.alloc(Highlight, row.render.len);
                    mem.copy(Highlight, saved_hl.?, row.hl);
                    @memset(row.hl[safe_match .. safe_match + qlen], Highlight.match);

                    self.cy = 0;
                    self.cx = safe_match;
                    self.row_offset = current;
                    self.col_offset = 0;

                    if (self.cx > self.screencols) {
                        var diff = self.cx - self.screencols;
                        self.cx -= diff;
                        self.col_offset += diff;
                    }
                }
            }
        }
    }

    // Load the specified program in the editor memory.
    fn open(self: *Self, file_path: []const u8) !void {
        self.file_path = file_path;
        const file = try fs.cwd().createFile(self.file_path, .{
            .read = true,
            .truncate = false,
        });

        defer file.close();

        var i: usize = 0;
        // Just read the entire file into memory... what could go wrong
        var file_bytes = try file.reader().readAllAlloc(self.allocator, std.math.maxInt(u32));
        var it = std.mem.split(u8, file_bytes, "\n");

        while (it.next()) |line| {
            try self.insertRow(i, line);
            i += 1;
        }
        self.dirty = false;
        return;
    }

    // Append the string 's' at the end of a row
    fn rowAppendString(self: *Self, row: *Row, s: []const u8) !void {
        var len = row.src.len;
        var s_len = s.len;
        row.src = try self.allocator.realloc(row.src[0..len], len + s_len);
        _ = self.allocator.resize(row.src[0..len], len + s_len);

        mem.copy(u8, row.src[len .. len + s_len], s);

        try self.updateRow(row);
        self.dirty = true;
    }

    fn delRow(self: *Self, at: usize) !void {
        if (at >= self.rows.items.len) return;

        _ = self.rows.orderedRemove(at);
        self.dirty = true;
    }

    fn delChar(self: *Self) !void {
        var file_row = self.row_offset + self.cy;
        var file_col = self.col_offset + self.cx;

        if (file_row >= self.rows.items.len or (file_col == 0 and file_row == 0)) return;

        var row = &self.rows.items[file_row];
        if (file_col == 0) {
            file_col = self.rows.items[file_row - 1].src.len;
            try self.rowAppendString(
                &self.rows.items[file_row - 1],
                row.src,
            );
            try self.delRow(file_row);

            if (self.cy == 0) self.row_offset -= 1 else self.cy -= 1;
            self.cx = file_col;

            if (self.cx >= self.screencols) {
                var shift: usize = self.screencols - self.cx + 1;
                self.cx -= shift;
                self.col_offset += shift;
            }
        } else {
            try self.rowDelChar(row, file_col - 1);
            if (self.cx == 0 and self.col_offset > 0) {
                self.col_offset -= 1;
            } else {
                self.cx -= 1;
            }
            try self.updateRow(row);
        }
    }

    // Delete the character at offset 'at' from the specified row.
    fn rowDelChar(self: *Self, row: *Row, at: usize) !void {
        if (row.src.len <= at) return;

        mem.copy(u8, row.src[at..row.src.len], row.src[at + 1 .. row.src.len]);
        try self.updateRow(row);
        row.src.len -= 1;
        self.dirty = true;
    }

    /// Insert a character at the specified position in a row, moving the remaining
    /// chars on the right if needed.
    fn rowInsertChar(self: *Self, row: *Row, at: usize, c: u8) !void {
        var old_src = try self.allocator.dupe(u8, row.src);
        row.src = try self.allocator.realloc(row.src, old_src.len + 1);

        if (at > row.src.len) {
            @memset(row.src[at .. at + 1], c);
        } else {
            var j: usize = 0;
            for (0..row.src.len) |i| {
                if (i == at) {
                    row.src[i] = c;
                } else {
                    row.src[i] = old_src[j];
                    j += 1;
                }
            }
        }

        try self.updateRow(row);
        self.dirty = true;
    }

    fn insertRow(self: *Self, at: usize, buf: []const u8) !void {
        if (at < 0 or at > self.rows.items.len) return;

        var row = Row{ .src = try self.allocator.dupe(u8, buf), .render = try self.allocator.alloc(u8, buf.len), .hl = try self.allocator.alloc(Highlight, buf.len) };

        @memset(row.hl, Highlight.normal);

        try self.updateRow(&row);
        try self.rows.insert(at, row);

        self.dirty = true;
    }

    // Update the rendered version.
    fn updateRow(self: *Self, row: *Row) !void {
        self.allocator.free(row.render);

        row.render = try self.allocator.dupe(u8, row.src);

        try self.updateSyntax(row);
    }

    fn fixCursor(self: *Self) void {
        if (self.cy == self.screenrows - 1) self.row_offset += 1 else self.cy += 1;

        self.cx = 0;
        self.col_offset = 0;
    }

    fn insertNewline(self: *Self) !void {
        var file_row = self.row_offset + self.cy;
        var file_col = self.col_offset + self.cx;

        if (file_row >= self.rows.items.len) {
            if (file_row == self.rows.items.len) {
                try self.insertRow(file_row, "");
                self.fixCursor();
            }
            return;
        }

        var row = &self.rows.items[file_row];
        if (file_col >= row.src.len) file_col = row.src.len;

        if (file_col == 0) {
            try self.insertRow(file_row, "");
        } else {
            try self.insertRow(file_row + 1, row.src[file_col..row.src.len]);

            // mem.trim_
            // row.*.src = mem.trimRight(u8, row.src, row.src[file_col..row.src.len]);
            var i: usize = 0;
            for (row.src[0..file_col]) |c| {
                row.src[i] = c;
                i += 1;
            }

            _ = self.allocator.resize(row.src, file_col);
            // mem.copy(u8, row.src[0..file_col], new_src);
            row.*.src.len = file_col;
            // update row
            try self.updateRow(row);
        }

        self.fixCursor();
    }

    /// Read a key from the terminal put in raw mode, trying to handle
    /// escape sequences.
    fn readKey(self: *Self) !u8 {
        var seq = try self.allocator.alloc(u8, 3);
        defer self.allocator.free(seq);
        _ = try os.read(os.linux.STDIN_FILENO, &self.c);

        switch (self.c[0]) {
            @enumToInt(Key.esc) => {
                _ = try os.read(os.linux.STDIN_FILENO, seq[0..1]);
                _ = try os.read(os.linux.STDIN_FILENO, seq[1..2]);

                if (seq[0] == '[') {
                    switch (seq[1]) {
                        '0'...'9' => {
                            _ = try os.read(os.linux.STDIN_FILENO, seq[2..3]);
                            if (seq[2] == '~') {
                                switch (seq[1]) {
                                    '1' => return @enumToInt(Key.home),
                                    '3' => return @enumToInt(Key.del),
                                    '4' => return @enumToInt(Key.end),
                                    '5' => return @enumToInt(Key.page_up),
                                    '6' => return @enumToInt(Key.page_down),
                                    '7' => return @enumToInt(Key.home),
                                    '8' => return @enumToInt(Key.end),
                                    else => {},
                                }
                            }
                        },
                        'A' => return @enumToInt(Key.arrow_up),
                        'B' => return @enumToInt(Key.arrow_down),
                        'C' => return @enumToInt(Key.arrow_right),
                        'D' => return @enumToInt(Key.arrow_left),
                        'H' => return @enumToInt(Key.home),
                        'F' => return @enumToInt(Key.end),
                        else => {},
                    }
                } else if (seq[0] == 'O') {
                    switch (seq[1]) {
                        'H' => return @enumToInt(Key.home),
                        'F' => return @enumToInt(Key.end),
                        else => {},
                    }
                }

                return @enumToInt(Key.esc);
            },
            else => return self.c[0],
        }

        return self.c[0];
    }

    fn rowsToString(self: *Self) ![]u8 {
        var len: usize = 0;
        for (self.rows.items) |row| {
            len += row.src.len + 1;
        }

        var buf = try self.allocator.alloc(u8, len);

        len = 0;
        var prev_len: usize = 0;
        for (self.rows.items) |row| {
            mem.copy(u8, buf[prev_len .. prev_len + row.src.len], row.src);
            mem.copy(u8, buf[prev_len + row.src.len .. prev_len + row.src.len + 1], "\n");
            prev_len += row.src.len + 1;
        }

        return buf;
    }

    // Save current file on disk.
    fn save(self: *Self) !void {
        const buf = try self.rowsToString();
        defer self.allocator.free(buf);

        const file = try fs.cwd().createFile(
            self.file_path,
            .{
                .read = true,
            },
        );
        defer file.close();

        file.writeAll(buf) catch |err| {
            return err;
        };

        try self.setStatusMessage("{d} bytes written on disk", .{buf.len});

        self.dirty = false;
        return;
    }

    fn processKeypress(self: *Self) !void {
        var c = try self.readKey();

        switch (@intToEnum(Key, c)) {
            .enter => return try self.insertNewline(),
            .ctrl_c => return,
            .ctrl_q => {
                if (self.dirty and self.quit_times > 0) {
                    try self.setStatusMessage(
                        "WARNING!!! File has unsaved changes. Press Ctrl-Q {d} more times to quit.",
                        .{self.quit_times},
                    );
                    self.quit_times -= 1;
                    return;
                }
                self.disableRawMode() catch unreachable;
                os.exit(0);
            },
            .ctrl_s => {
                self.save() catch |err| {
                    try self.setStatusMessage("Can't save! I/O error: {any}", .{err});
                };
            },
            .ctrl_f => try self.find(),
            .backspace, .ctrl_h, .del => {
                if (@intToEnum(Key, c) == .del) self.moveCursor(@enumToInt(Key.arrow_right));
                try self.delChar();
            },
            .arrow_left, .arrow_up, .arrow_down, .arrow_right => self.moveCursor(c),
            .esc, .ctrl_l => return,
            .home => self.cx = 0,
            .end => {
                if (self.cy < self.rows.items.len) self.cx = self.rows.items[self.cy].src.len;
            },
            .page_up, .page_down => |pg| {
                if (pg == .page_up and self.cy != 0) {
                    self.cy = 0;
                } else if (pg == .page_down and self.cy != self.screenrows - 1) {
                    self.cy = self.screenrows - 1;
                }

                var direction: Key =
                    if (pg == Key.page_up) .arrow_up else .arrow_down;
                for (0..self.screenrows - 1) |_| {
                    self.moveCursor(@enumToInt(direction));
                }
            },
            else => try self.insertChar(c),
        }

        self.quit_times = GRAM_QUIT_TIMES; // Reset it to the original value.
    }

    // Insert 'c' at the current prompt position.
    fn insertChar(self: *Self, c: u8) !void {
        var file_row = self.row_offset + self.cy;
        var file_col = self.col_offset + self.cx;

        if (file_row >= self.rows.items.len) {
            for (self.rows.items.len..file_row + 1) |_| try self.insertRow(self.rows.items.len, "");
        }

        try self.rowInsertChar(&self.rows.items[file_row], file_col, c);

        if (self.cx == self.screencols - 1) self.col_offset += 1 else self.cx += 1;
    }

    fn enableRawMode(self: *Self) !void {
        if (self.raw_mode) return;

        const VMIN = 5;
        const VTIME = 6;

        self.orig_termios = try os.tcgetattr(os.STDIN_FILENO); // So we can restore later
        var termios = self.orig_termios;

        // input modes: no break, no CR to NL, no parity check, no strip char, no start/stop output ctrl.
        termios.iflag &= ~(os.linux.BRKINT | os.linux.ICRNL | os.linux.INPCK | os.linux.ISTRIP | os.linux.IXON);
        // output modes: disable post processing
        termios.oflag &= ~(os.linux.OPOST);
        // control modes: set 8 bit chars
        termios.cflag |= os.linux.CS8;
        // local modes: choign off, canonical off, no extended functions, no signal chars (^Z, ^C)
        termios.lflag &= ~(os.linux.ECHO | os.linux.ICANON | os.linux.IEXTEN | os.linux.ISIG);
        termios.cc[VMIN] = 0;
        termios.cc[VTIME] = 1;

        _ = os.linux.tcsetattr(os.linux.STDIN_FILENO, .FLUSH, &termios);
        self.raw_mode = true;
    }

    fn disableRawMode(self: *Self) !void {
        if (self.raw_mode) {
            _ = os.linux.tcsetattr(os.linux.STDIN_FILENO, .FLUSH, &self.orig_termios);
            self.raw_mode = false;
        }
    }

    fn deinit(self: *Self) void {
        self.disableRawMode() catch unreachable;
        for (self.rows.items) |row| {
            self.allocator.free(row.src);
            self.allocator.free(row.render);
            self.allocator.free(row.hl);
        }
        self.rows.deinit();
    }

    // Writes the whole screen using VT100 escape characters.
    fn refreshScreen(self: *Self) !void {
        var ab = ArrayList(u8).init(self.allocator);
        defer ab.deinit();

        try ab.appendSlice("\x1b[?25l"); // Hide cursor
        try ab.appendSlice("\x1b[H");

        // Draw rows
        for (0..self.screenrows) |y| {
            var file_row = self.row_offset + y;

            if (file_row >= self.rows.items.len) {
                if (self.rows.items.len == 0 and y == self.screenrows / 3) {
                    var buf: [32]u8 = undefined;

                    var welcome = try std.fmt.bufPrint(&buf, "Gram editor -- version {s}\x1b[0K\r\n", .{GRAM_VERSION});
                    var padding: usize = if (welcome.len > self.screencols) 0 else (self.screencols - welcome.len) / 2;
                    for (0..padding) |_| try ab.appendSlice(" ");
                    try ab.appendSlice(welcome);
                } else {
                    try ab.appendSlice("~\x1b[0K\r\n");
                }
            } else {
                var row = &self.rows.items[file_row];
                var len = if (row.render.len <= self.col_offset) 0 else row.render.len - self.col_offset;
                var current_color: u8 = 0;

                if (len > 0) {
                    if (len > self.screencols) len = self.screencols;

                    var start = self.col_offset;
                    for (0..len) |j| {
                        var hl = row.hl[j];
                        switch (hl) {
                            Highlight.normal => {
                                if (current_color > 0) {
                                    try ab.appendSlice("\x1b[39m");
                                    current_color = 0;
                                }

                                try ab.appendSlice(row.render[start + j .. start + j + 1]);
                            },
                            else => {
                                var color = @enumToInt(hl);
                                if (color != current_color) {
                                    var buf: [16]u8 = undefined;

                                    current_color = color;
                                    try ab.appendSlice(try std.fmt.bufPrint(&buf, "\x1b[{d}m", .{color}));
                                }
                                try ab.appendSlice(row.render[start + j .. start + j + 1]);
                            },
                        }
                    }
                }
                try ab.appendSlice("\x1b[39m");
                try ab.appendSlice("\x1b[0K");
                try ab.appendSlice("\r\n");
            }
        }

        // Create a two status rows status. First row:
        try ab.appendSlice("\x1b[0K");
        try ab.appendSlice("\x1b[7m");
        var rstatus: [80]u8 = undefined;
        var modified: []const u8 = if (self.dirty) "(modified)" else "";

        var status = try std.fmt.allocPrint(self.allocator, "{s} - {d} lines {s}", .{
            self.file_path,
            self.rows.items.len,
            modified,
        });
        var len = if (status.len > self.screencols) self.screencols else status.len;
        _ = try std.fmt.bufPrint(&rstatus, "{d}/{d}", .{
            self.row_offset + self.cy + 1,
            self.rows.items.len,
        });
        try ab.appendSlice(status[0..status.len]);

        for (len..self.screencols) |_| {
            if (self.screencols - len == rstatus.len) {
                try ab.appendSlice(&rstatus);
                break;
            } else {
                try ab.appendSlice(" ");
            }
        }
        try ab.appendSlice("\x1b[0m\r\n");

        // Second row
        try ab.appendSlice("\x1b[0K");
        try ab.appendSlice(self.status_message.items);

        // Draw cursor
        var buf: [32]u8 = undefined;
        var cx: usize = 1;
        var file_row = self.row_offset + self.cy;

        if (file_row < self.rows.items.len) {
            var row = self.rows.items[file_row];
            for (self.col_offset..self.col_offset + self.cx) |j| {
                if (j < row.src.len and row.src[j] == '\t') cx += 7 - (cx % 8);
                cx += 1;
            }
        }
        try ab.appendSlice(try std.fmt.bufPrint(&buf, "\x1b[{d};{d}H", .{ self.cy + 1, cx }));
        try ab.appendSlice("\x1b[?25h");

        _ = try os.write(os.linux.STDOUT_FILENO, ab.items);
    }

    fn moveCursor(self: *Self, c: u8) void {
        var file_row = self.row_offset + self.cy;
        var file_col = self.col_offset + self.cx;

        switch (@intToEnum(Key, c)) {
            .arrow_left => {
                if (self.cx == 0) {
                    if (self.col_offset > 0) {
                        self.col_offset -= 1;
                    } else {
                        if (file_row > 0) {
                            self.cy -= 1;
                            self.cx = self.rows.items[file_row - 1].src.len;
                            if (self.cx > self.screencols - 1) {
                                self.col_offset = self.cx - self.screencols + 1;
                                self.cx = self.screencols - 1;
                            }
                        }
                    }
                } else {
                    self.cx -= 1;
                }
            },
            .arrow_right => {
                if (file_row < self.rows.items.len) {
                    var row = self.rows.items[file_row];

                    if (file_col < row.src.len) {
                        if (self.cx == self.screencols - 1) self.col_offset += 1 else self.cx += 1;
                    } else if (file_col == row.src.len) {
                        self.cx = 0;
                        self.col_offset = 0;

                        if (self.cy == self.screenrows - 1) self.row_offset += 1 else self.cy += 1;
                    }
                }
            },
            .arrow_up => {
                if (self.cy == 0) {
                    if (self.row_offset > 0) self.row_offset -= 1;
                } else {
                    self.cy -= 1;
                }
            },
            .arrow_down => {
                if (file_row < self.rows.items.len) {
                    if (self.cy == self.screenrows - 1) self.row_offset += 1 else self.cy += 1;
                }
            },
            else => unreachable,
        }

        file_row = self.row_offset + self.cy;
        file_col = self.col_offset + self.cx;
        var row_len: usize = if (file_row >= self.rows.items.len) 0 else self.rows.items[file_row].src.len;
        if (file_col > row_len) {
            self.cx -= file_col - row_len;

            if (self.cx < 0) {
                self.col_offset += self.cx;
                self.cx = 0;
            }
        }
    }

    fn getCursorPosition(self: *Self) !void {
        var buf: [32]u8 = undefined;

        _ = try os.write(os.linux.STDOUT_FILENO, "\x1b[6n");

        for (0..buf.len - 1) |i| {
            _ = os.read(os.linux.STDIN_FILENO, &buf) catch break;
            if (buf[i] == 'R') break;
        }

        if (buf[0] != '\x1b' or buf[1] != '[') return error.CursorError;
        _ = try self.readKey();
    }

    fn setStatusMessage(self: *Self, comptime format: []const u8, args: anytype) !void {
        self.status_message.clearRetainingCapacity();
        var buf = try std.fmt.allocPrint(self.allocator, format, args);
        try self.status_message.appendSlice(buf);
    }
};

pub fn main() !void {
    var args = std.process.args();
    _ = args.next(); // ignore self, then read file name
    var file_path = args.next() orelse {
        std.debug.print("Usage: gram [file_name]\n\n", .{});
        return error.NoFileName;
    };
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var editor = try Editor.init(allocator);
    defer editor.deinit();
    try editor.updateWindowSize();
    try editor.open(file_path);

    try editor.enableRawMode();
    try editor.setStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find", .{});
    while (true) {
        try editor.refreshScreen();
        try editor.processKeypress();
    }
}
