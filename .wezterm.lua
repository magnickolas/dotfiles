local wezterm = require("wezterm")
-- local gpus = wezterm.gui.enumerate_gpus()
return {
  default_prog = { "/usr/bin/zsh", "-l" },
  default_cursor_style = "SteadyBlock",
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
  -- webgpu_preferred_adapter = gpus[1],
  -- front_end = 'WebGpu',
  -- webgpu_power_preference = "HighPerformance",
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
  window_background_opacity = 0.8,
  hide_tab_bar_if_only_one_tab = true,
  window_close_confirmation = "NeverPrompt",
}
