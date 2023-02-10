local function concat(t1, t2)
	for _, v in ipairs(t2) do
		table.insert(t1, v)
	end
	return t1
end

local wezterm = require("wezterm")
local act = wezterm.action
local gpus = wezterm.gui.enumerate_gpus()
local move_tabs_keys = {}
for i = 1, 8 do
	-- CTRL+ALT + number to move to that position
	table.insert(move_tabs_keys, {
		key = tostring(i),
		mods = "CTRL|ALT",
		action = wezterm.action.MoveTab(i - 1),
	})
end

local keys = concat(move_tabs_keys, {
	-- This will create a new split and run your default program inside it
	{
		key = ")",
		mods = "CTRL|SHIFT",
		action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
	},
	{
		key = "(",
		mods = "CTRL|SHIFT",
		action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
	},
})

return {
	default_prog = { "/usr/bin/zsh", "-l" },
	default_cursor_style = "SteadyBar",
	font = wezterm.font_with_fallback({
		{
			family = "Iosevka Term",
			weight = "Regular",
			harfbuzz_features = { "kern=1", "liga=1", "clig=1", "calt=1" },
		},
		{ family = "Twemoji" },
		{ family = "Symbols Nerd Font Mono" },
		{ family = "DejaVu Sans Mono" },
	}),
	warn_about_missing_glyphs = false,
	custom_block_glyphs = true,
	webgpu_preferred_adapter = gpus[1],
	front_end = "WebGpu",
	webgpu_power_preference = "HighPerformance",
	font_size = 24.0,
	adjust_window_size_when_changing_font_size = false,
	dpi = 96.0,
	window_padding = {
		left = 0,
		right = 0,
		top = 0,
		bottom = 0,
	},
	font_shaper = "Harfbuzz",
	harfbuzz_features = { "kern", "liga", "clig", "calt" },
	color_scheme = "Gruvbox dark, hard (base16)",
	window_background_opacity = 0.69,
	hide_tab_bar_if_only_one_tab = true,
	window_close_confirmation = "NeverPrompt",
	keys = keys,

	mouse_bindings = {
		-- Scrolling up while holding CTRL increases the font size
		{
			event = { Down = { streak = 1, button = { WheelUp = 1 } } },
			mods = "CTRL",
			action = act.IncreaseFontSize,
		},

		-- Scrolling down while holding CTRL decreases the font size
		{
			event = { Down = { streak = 1, button = { WheelDown = 1 } } },
			mods = "CTRL",
			action = act.DecreaseFontSize,
		},
	},
}
