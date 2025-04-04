local wezterm = require("wezterm")

config = wezterm.config_builder()

config = {
    automatically_reload_config = true,
    enable_tab_bar = false,
    window_close_confirmation = "NeverPrompt",
    window_decorations = "RESIZE",
    color_scheme = 'OneDark (base16)',
    window_background_opacity = 0.9,
    text_background_opacity = 1.0,
    font = wezterm.font 'Maple Mono NF CN',
}

return config
