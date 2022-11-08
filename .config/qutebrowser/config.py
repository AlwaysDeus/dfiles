config.load_autoconfig(False)


## Colorscheme
import dracula.draw
# Load existing settings made via :set
config.load_autoconfig()
dracula.draw.blood(c, {
    'spacing': {
        'vertical': 3,
        'horizontal': 8
    }
})

## Dark mode
config.set("colors.webpage.preferred_color_scheme", "dark")
config.set("colors.webpage.darkmode.policy.images", "never")
config.set("colors.webpage.darkmode.enabled", True)

## Statusbar
config.set('statusbar.show', 'in-mode')
config.set('statusbar.position', 'top')
## Hide cursor in normal mode after changing from insert mode
config.bind('<Escape>', 'mode-leave ;; jseval -q document.activeElement.blur()', mode='insert')

## Key bindings
config.bind('M', 'hint links spawn mpv {hint-url}')
config.bind('xx', 'config-cycle tabs.show always never')

## Disable autoplay
config.set('content.autoplay', False)
#config.set('content.prefers_reduced_motion', True)
