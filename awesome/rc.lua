-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")

local centerwork = require "centerwork"

require "keybindings"
root.keys(Globalkeys)

-- Set theme
beautiful.init(awful.util.getdir("config") .. "/themes/default/theme.lua")


Terminal = "kitty"
Editor = os.getenv("EDITOR") or "vim"
Editor_cmd = Terminal .. " -e " .. Editor


-- Hack to remove the tmux and firefox key bindings from the hotkeys
package.loaded["awful.hotkeys_popup.keys.tmux"] = {}
package.loaded["awful.hotkeys_popup.keys.firefox"] = {}
-- Then enable hotkeys
require("awful.hotkeys_popup.keys")


-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    centerwork,
    awful.layout.suit.floating,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.magnifier
}


-- {{{ Wibar
-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

-- Create a textclock widget
local mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(t)
            t:view_only()
        end
    ),
    awful.button(
        {modkey},
        1,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end
    ),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button(
        {modkey},
        3,
        function(t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end
    ),
    awful.button(
        {},
        4,
        function(t)
            awful.tag.viewnext(t.screen)
        end
    ),
    awful.button(
        {},
        5,
        function(t)
            awful.tag.viewprev(t.screen)
        end
    )
)

local tasklist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            if c == client.focus then
                c.minimized = true
            else
                c:emit_signal("request::activate", "tasklist", {raise = true})
            end
        end
    ),
    awful.button(
        {},
        3,
        function()
            awful.menu.client_list({theme = {width = 250}})
        end
    ),
    awful.button(
        {},
        4,
        function()
            awful.client.focus.byidx(1)
        end
    ),
    awful.button(
        {},
        5,
        function()
            awful.client.focus.byidx(-1)
        end
    )
)

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

local pavuctl = wibox.widget.imagebox()
pavuctl:set_image("/usr/share/icons/Adwaita/scalable/apps/multimedia-volume-control-symbolic.svg")
    pavuctl:connect_signal("button::press", function()
    awful.spawn("pavucontrol")
end)

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(
    function(s)
        -- Wallpaper
        set_wallpaper(s)

        -- Each screen has its own tag table.
        -- awful.tag({"", "", ""}, s, awful.layout.layouts[1])
        awful.tag.add("", {layout = awful.layout.layouts[1], screen = s, selected = true})
        awful.tag.add("", {layout = awful.layout.layouts[2], screen = s})
        awful.tag.add("", {layout = awful.layout.layouts[2], screen = s})

        -- Create a promptbox for each screen
        s.mypromptbox = awful.widget.prompt()

        s.mylayoutbox = wibox.container.margin(awful.widget.layoutbox(s),5,15,5,5)
        s.mylayoutbox:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        awful.layout.inc(1)
                    end
                ),
                awful.button(
                    {},
                    3,
                    function()
                        awful.layout.inc(-1)
                    end
                )
            )
        )

        s.pavucontrolbox = wibox.container.margin(pavuctl,5,15,5,5)


        -- Create a taglist widget
        s.mytaglist = wibox.container.margin(
            awful.widget.taglist {
            screen = s,
            filter = awful.widget.taglist.filter.all,
            buttons = taglist_buttons
        },15,5,5,5)

        -- Create a tasklist widget
        s.mytasklist =
            awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons
        }

        s.mysystray = wibox.container.margin(wibox.widget.systray{},5, 5, 5, 5)

        -- Create the wibox
        s.mywibox = awful.wibar({
            position = "top",
            screen = s,
            width = 1000,
            stretch = false,
            shape = gears.shape.rounded_bar,
        })
        s.mywibox.y = 10

        -- Add widgets to the wibox
        s.mywibox:setup {
            layout = wibox.layout.align.horizontal,
            {
                -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                -- mylauncher,
                s.mytaglist,
                s.mypromptbox
            },
            -- s.mytasklist, -- Middle widget
            s.space,
            {
                -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                s.pavucontrolbox,
                s.mysystray,
                mykeyboardlayout,
                mytextclock,
                s.mylayoutbox,
            }
        }
    end
)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = Clientkeys,
            buttons = Clientbuttons,
            screen = awful.screen.preferred,
            placement = awful.placement.centered
        }
    },
    -- Floating clients.
    {
        rule_any = {
            instance = {
                "pinentry"
            },
            class = {
                "feh",
                "mpv",
                "Blueberry.py",
                "Zathura",
                "Virt-manager",
                "Remote-viewer",
                "Spotify",
                "Pavucontrol",
                "Nm-connection-editor",
                "Microsoft Teams - Preview",
                "privateinternetaccess",
                "Org.gnome.Nautilus",
                "Google-chrome",
                "Signal",
                "Protonvpn",
                "Display",
                "Evince",
                "Eog"
            },
            name = {
                "Event Tester" -- xev.
            },
            role = {
                "AlarmWindow", -- Thunderbird's calendar.
                "ConfigManager", -- Thunderbird's about:config.
                "pop-up" -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = {floating = true}
    },
    -- Add titlebars to normal clients and dialogs
    {
        rule_any = {
            type = {"normal", "dialog"}
        },
        properties = {titlebars_enabled = true}
    },

    {rule = { class = "Slack", name = "screen share" }, properties = { floating = true, ontop = true }},
    -- -- Set Signal and spotify to always be on the tag named "2"
    -- { rule_any = { class = {"Signal", "Spotify"} }, properties = { tag = "2"}},
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
    "manage",
    function(c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- if not awesome.startup then awful.client.setslave(c) end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end

        -- Rounded corners for every client
        c.shape = gears.shape.rounded_rect
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    "request::titlebars",
    function(c)
        -- buttons for the titlebar
        local buttons =
            gears.table.join(
            awful.button(
                {},
                1,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.move(c)
                end
            ),
            awful.button(
                {},
                3,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.resize(c)
                end
            )
        )

        awful.titlebar(c):setup {
            {
                -- Left
                awful.titlebar.widget.iconwidget(c),
                buttons = buttons,
                layout = wibox.layout.fixed.horizontal
            },
            {
                -- Middle
                {
                    -- Title
                    align = "center",
                    widget = awful.titlebar.widget.titlewidget(c)
                },
                buttons = buttons,
                layout = wibox.layout.flex.horizontal
            },
            {
                -- Right
                awful.titlebar.widget.floatingbutton(c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.stickybutton(c),
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.closebutton(c),
                layout = wibox.layout.fixed.horizontal()
            },
            layout = wibox.layout.align.horizontal
        }

        -- hide titlebar on startup
        awful.titlebar.hide(c)
    end
)


-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
    "mouse::enter",
    function(c)
        c:emit_signal("request::activate", "mouse_enter", {raise = false})
    end
)

client.connect_signal(
    "focus",
    function(c)
        c.border_color = beautiful.border_focus
    end
)

client.connect_signal(
    "unfocus",
    function(c)
        c.border_color = beautiful.border_normal
    end
)
-- }}}

-- Notifications
for _, preset in pairs(naughty.config.presets) do
    preset.position = "top_middle"
    preset.margin = 10
    preset.icon_size = 32
    preset.border_width = 2
end

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify(
        {
            preset = naughty.config.presets.critical,
            title = "Oops, there were errors during startup!",
            text = awesome.startup_errors
        }
    )
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal(
        "debug::error",
        function(err)
            -- Make sure we don't go into an endless error loop
            if in_error then
                return
            end
            in_error = true

            naughty.notify(
                {
                    preset = naughty.config.presets.critical,
                    title = "Oops, an error happened!",
                    text = tostring(err)
                }
            )
            in_error = false
        end
    )
end
-- }}}
