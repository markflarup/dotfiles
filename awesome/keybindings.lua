-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")

local modkey = "Mod4"

Globalkeys =
    gears.table.join(
    awful.key({modkey}, "F1", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),
    -- awful.key({modkey}, "Left", awful.tag.viewprev, {description = "view previous", group = "tag"}),
    -- awful.key({modkey}, "Right", awful.tag.viewnext, {description = "view next", group = "tag"}),
    awful.key({modkey}, "Escape", awful.tag.history.restore, {description = "go back", group = "tag"}),
    awful.key(
        {modkey},
        "Tab",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "Tab",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key(
        {modkey},
        "Left",
        function()
            awful.client.focus.bydirection("left")
        end,
        {description = "focus window left", group = "client"}
    ),
    awful.key(
        {modkey},
        "Right",
        function()
            awful.client.focus.bydirection("right")
        end,
        {description = "focus window right", group = "client"}
    ),
    awful.key(
        {modkey},
        "Up",
        function()
            awful.client.focus.bydirection("up")
        end,
        {description = "focus window above", group = "client"}
    ),
    awful.key(
        {modkey},
        "Down",
        function()
            awful.client.focus.bydirection("down")
        end,
        {description = "focus window below", group = "client"}
    ),
    --awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
    --          {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key(
        {modkey, "Shift"},
        "Left",
        function()
            awful.client.swap.bydirection("left")
        end,
        {description = "swap position with client on the left", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "Right",
        function()
            awful.client.swap.bydirection("right")
        end,
        {description = "swap position with client on the right", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "Up",
        function()
            awful.client.swap.bydirection("up")
        end,
        {description = "swap position with client above", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "Down",
        function()
            awful.client.swap.bydirection("down")
        end,
        {description = "swap position with client below", group = "client"}
    ),
    -- awful.key(
    --     {modkey, "Control"},
    --     "Right",
    --     function()
    --         awful.screen.focus_relative(1)
    --     end,
    --     {description = "focus the next screen", group = "screen"}
    -- ),
    -- awful.key(
    --     {modkey, "Control"},
    --     "Left",
    --     function()
    --         awful.screen.focus_relative(-1)
    --     end,
    --     {description = "focus the previous screen", group = "screen"}
    -- ),
    awful.key({modkey}, "u", awful.client.urgent.jumpto, {description = "jump to urgent client", group = "client"}),
    -- Standard program
    awful.key(
        {modkey, "Shift"},
        "Return",
        function()
            awful.spawn(Terminal)
        end,
        {description = "open a terminal", group = "launcher"}
    ),
    awful.key({modkey, "Control"}, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({modkey, "Shift"}, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),
    awful.key(
        {modkey},
        "l",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = "increase master width factor", group = "layout"}
    ),
    awful.key(
        {modkey},
        "h",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = "decrease master width factor", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "h",
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = "increase the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "l",
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = "decrease the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "h",
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = "increase the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "l",
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = "decrease the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next", group = "layout"}
    ),
    -- Prompt
    awful.key(
        {modkey},
        "r",
        function()
            awful.screen.focused().mypromptbox:run()
        end,
        {description = "run prompt", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "x",
        function()
            awful.prompt.run {
                prompt = "Run Lua code: ",
                textbox = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        {description = "lua execute prompt", group = "awesome"}
    ),
    -- Menubar
    awful.key(
        {modkey},
        "p",
        function()
            awful.spawn("rofi -show drun")
        end,
        {description = "show rofi run", group = "launcher"}
    ),
    -- Spotify
    awful.key(
        {"Control"},
        "F7",
        function()
            awful.spawn(
                "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
            )
        end,
        {description = "play/pause", group = "spotify"}
    ),
    awful.key(
        {"Control"},
        "F8",
        function()
            awful.spawn(
                "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
            )
        end,
        {description = "next", group = "spotify"}
    ),
    awful.key(
        {"Control"},
        "F6",
        function()
            awful.spawn(
                "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
            )
        end,
        {description = "previous", group = "spotify"}
    ),
    awful.key(
        {modkey},
        "F11",
        function()
            awful.spawn.with_shell("~/Scripts/screenshot save_to_file")
        end,
        {description = "Save selection to file", group = "screenshot"}
    ),
    awful.key(
        {modkey, "Shift"},
        "F11",
        function()
            awful.spawn.with_shell("~/Scripts/screenshot")
        end,
        {description = "Save selection to clipboard", group = "screenshot"}
    )
)

Clientkeys =
    gears.table.join(
    awful.key(
        {modkey},
        "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "t",
        function(c)
            awful.titlebar.toggle(c)
        end,
        {description = "toggle titlebar", group = "client"}
    ),
    awful.key(
        {modkey},
        "q",
        function(c)
            c:kill()
        end,
        {description = "close", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "space",
        awful.client.floating.toggle,
        {description = "toggle floating", group = "client"}
    ),
    awful.key(
        {modkey},
        "Return",
        function(c)
            c:swap(awful.client.getmaster())
        end,
        {description = "move to master", group = "client"}
    ),
    -- awful.key(
    --     {modkey},
    --     "o",
    --     function(c)
    --         c:move_to_screen()
    --     end,
    --     {description = "move to screen", group = "client"}
    -- ),
    awful.key(
        {modkey},
        "s",
        function(c)
            c.sticky = not c.sticky
        end,
        {description = "toggle sticky", group = "client"}
    ),
    awful.key(
        {modkey},
        "t",
        function(c)
            c.ontop = not c.ontop
        end,
        {description = "toggle keep on top", group = "client"}
    ),
    awful.key(
        {modkey},
        "m",
        function(c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end,
        {description = "minimize", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "m",
        function()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                c:emit_signal("request::activate", "key.unminimize", {raise = true})
            end
        end,
        {description = "restore minimized", group = "client"}
    )
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    Globalkeys =
        gears.table.join(
        Globalkeys,
        -- View tag only.
        awful.key(
            {modkey},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            {description = "view tag #" .. i, group = "tag"}
        ),
        -- Toggle tag display.
        awful.key(
            {modkey, "Control"},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            {description = "toggle tag #" .. i, group = "tag"}
        ),
        -- Move client to tag.
        awful.key(
            {modkey, "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            {description = "move focused client to tag #" .. i, group = "tag"}
        ),
        -- Toggle tag on focused client.
        awful.key(
            {modkey, "Control", "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            {description = "toggle focused client on tag #" .. i, group = "tag"}
        )
    )
end


Clientbuttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
        end
    ),
    awful.button(
        {modkey},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.move(c)
        end
    ),
    awful.button(
        {modkey},
        3,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.resize(c)
        end
    )
)
