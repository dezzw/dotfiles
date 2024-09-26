local wezterm = require("wezterm")

config = wezterm.config_builder()

config = {
    automatically_reload_config = true,
    enable_tab_bar = false,
    window_close_confirmation = "NeverPrompt",
    window_decorations = "RESIZE",
    color_scheme = "One Dark (Gogh)",
    window_background_opacity = 0.9,
    text_background_opacity = 1.0,
}

return config
