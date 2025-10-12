# https://qutebrowser.org/
# https://raw.githubusercontent.com/qutebrowser/qutebrowser/main/doc/img/cheatsheet-big.png
# :)

import os
import socket
import operator
import subprocess
import time
import urllib.request
from pathlib import Path
from shutil import which
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer # noqa: F401
from qutebrowser.api import interceptor, message

config = config  # type: ConfigAPI # noqa: F821
c = c            # type: ConfigContainer # noqa: F821
config.load_autoconfig(False)

# stolen from https://github.com/noctuid/dotfiles/blob/master/browsing/.config/qutebrowser/config.py
def nmap(key, command):
    """Bind key to command in normal mode."""
    config.bind(key, command, mode='normal')

# ui
c.completion.scrollbar.width = 10
c.tabs.position = 'top'
c.tabs.show = 'multiple'
c.tabs.indicator.width = 0
c.tabs.title.format = '{audio}{current_title}'
c.tabs.title.alignment = 'center'
c.downloads.position = 'bottom'
# c.tabs.favicons.show = 'never'
c.tabs.favicons.show = 'always'

# behavior
c.downloads.location.prompt = False
c.hints.scatter = False
c.url.searchengines = {'DEFAULT': 'https://google.com/search?q={}' }
c.input.insert_mode.auto_load = True
c.input.insert_mode.auto_leave = False
c.tabs.background = True

if 'EDITOR' in os.environ:
    c.editor.command = [os.environ['EDITOR'] + ' "{}"']

if which("qutebrowser-edit"):
    c.editor.command = [which("qutebrowser-edit"), '-l{line}', '-c{column}', '-f{file}']

c.auto_save.session = True

nmap('b', 'cmd-set-text --space :tab-select')

# match 's' avy hinting in emacs
config.unbind('sf');
config.unbind('sk');
config.unbind('sl');
config.unbind('ss');

nmap('s', 'hint')

# kill the old habit:
config.unbind('f');

# colemak, and sync session
c.hints.chars = 'arstgkneio'
nmap('n', 'scroll-page 0 0.2')
nmap('e', 'scroll-page 0 -0.2')
nmap('N', 'tab-next')
nmap('E', 'tab-prev')
nmap('k', 'search-next')
nmap('K', 'search-prev')

nmap('d', 'tab-close')
nmap('u', 'undo')

# save_session = 'set messages.timeout 0;; session-save --force _autosave'

# this is nice, but the message is annoying - can we not?
nmap('N', 'tab-next  ;; session-save --force _autosave')
nmap('E', 'tab-prev  ;; session-save --force _autosave')
nmap('d', 'tab-close ;; session-save --force _autosave')
nmap('u', 'undo      ;; session-save --force _autosave')

# default theme:
theme = {
    'panel': {
        'height': 22,
    },
    'fonts': {
        'tabbar': 'monospace',
        'completion': 'monospace',
        'completion_size': 13,
        'tab_bold': False,
        'tab_size': 14,
        'status_size': 13,
    },
    'colors': {
        'bg': {
            'normal': '#f2e9e3',
            'weak': '#ddd4d3',
            'strong': '#d1c8cd',
            'focused': '#e0c4bf',
        },
        'fg': {
            'normal': '#544b45',
            'weak': '#473e3d',
            'strong': '#3f363b',
            'focused': '#544b45',
            'match': '#916156', # completion and hints (rename this alt?)
            'faded': '#67625f', # unfocused tabs
        },
    }
}

# import templated theme when file exists
themefile = os.environ["HOME"] + '/.config/qutebrowser/colors.py'
if os.path.exists(themefile):
    exec(Path(themefile).read_text())

cssfile = os.environ["HOME"] + '/.config/qutebrowser/settings.css'
if os.path.exists(cssfile):
    c.content.user_stylesheets = [cssfile]

colors = theme['colors']

# apply the theme:
def setToBG(colortype, target):
    config.set('colors.' + target, colors['bg'][colortype])

def setToFG(colortype, target):
    config.set('colors.' + target, colors['fg'][colortype])

def colorSync(colortype, setting):
    if setting.endswith('.fg'):
        setToFG(colortype, setting)
    elif setting.endswith('.bg'):
        setToBG(colortype, setting)
    elif setting.endswith('.top') or setting.endswith('.bottom'):
        setToFG(colortype, setting)
    else:
        setToFG(colortype, setting + '.fg')
        setToBG(colortype, setting + '.bg')

# normal, weak, strong, focused
targets = {
    'normal' : [
        'statusbar.url.fg',
        'statusbar.normal',
        'statusbar.command',
        'statusbar.url.success.http.fg',
        'statusbar.url.success.https.fg',
        'hints',
        'downloads.bar.bg',
        'completion.category',

        'tabs.even.bg',
        'tabs.odd.bg',

        # prev: weak
        'completion.scrollbar',
        'downloads.start',
        'messages.info',
        'completion.fg',
        'completion.odd.bg',
        'completion.even.bg',
    ],

    'focused' : [
        'tabs.selected.even',
        'tabs.selected.odd',
        'statusbar.insert',
        'downloads.stop',
        'prompts',
        'messages.warning',
        'messages.error',
        'statusbar.url.hover.fg',
        'completion.item.selected',

        'contextmenu.selected',

        'completion.category.border.top',
        'completion.category.border.bottom',

        'completion.item.selected.border.top',
        'completion.item.selected.border.bottom',

    ],

    'weak': [
        'contextmenu.menu',
    ],

    'match': [
        'completion.match.fg',
        'hints.match.fg',
    ],

    'faded': [
        'tabs.even.fg',
        'tabs.odd.fg',
        'contextmenu.disabled.fg'
    ]
}

for colortype in targets:
    for target in targets[colortype]:
        colorSync(colortype, target)

setToFG('focused', 'statusbar.progress.bg')

config.set('hints.border', '1px solid ' + colors['fg']['normal'])

# tabbar
def makePadding(top, bottom, left, right):
    return { 'top': top, 'bottom': bottom, 'left': left , 'right': right }

# this doesn't work?? the value is 2small
# font_height = {{{txth -f "$q_tab_font" -s $q_tab_fontsize ph}}}

# variable: elisp (with-current-buffer (first (ns/buffers-by-mode 'org-mode)) (line-pixel-height))
font_height = 27
surround = round((theme['panel']['height'] - font_height) / 2)
if surround < 1:
    surround = 0

c.tabs.padding = makePadding(surround, surround, 8, 8)
c.tabs.indicator.padding = makePadding(0, 0, 0, 0)
c.statusbar.padding = makePadding(4, 4, 0, 0)

# fonts
fonts = theme['fonts']
# c.fonts.monospace = fonts['tabbar']

def GetSize(fontType):
    return str(fonts[fontType + '_size']) + 'pt '

tabFont = GetSize('tab') + fonts['tabbar']

# placement matters: bold handling:
c.fonts.hints = 'bold {0}'.format(tabFont)

if fonts['tab_bold']:
    tabFont = 'bold {0}'.format(tabFont)

# c.fonts.tabs = tabFont

# next stable qutebrowser version:
c.fonts.tabs.selected = tabFont
c.fonts.tabs.unselected = tabFont
c.fonts.contextmenu = tabFont

c.hints.radius = 0

c.fonts.completion.entry = GetSize('completion') + fonts['completion']
c.fonts.statusbar = GetSize('completion') + fonts['completion']

# nmap('<F9>', 'spawn --userscript jira_refine_copy')
nmap('<F10>', 'spawn --userscript chrome_open')
nmap('<F9>', 'spawn --userscript scrape')
nmap('<F12>', 'devtools')

REDIRECT_MAP = {
    # "i.reddit.com": operator.methodcaller('setHost', 'i.reddit.com'),
    # "preview.reddit.com": operator.methodcaller('setHost', 'reddit.com'),

    "reddit.com": operator.methodcaller('setHost', 'old.reddit.com'),
    "www.reddit.com": operator.methodcaller('setHost', 'old.reddit.com'),
}

def redirect_intercept(info):
    """Block the given request if necessary."""
    if (info.resource_type != interceptor.ResourceType.main_frame
            or info.request_url.scheme() in {"data", "blob"}):
        return

    url = info.request_url
    # message.info(url.host())
    redir = REDIRECT_MAP.get(url.host())
    if redir is not None and redir(url) is not False:
        message.info("Redirecting to " + url.toString())
        info.redirect(url)

interceptor.register(redirect_intercept)

def pull_adblock(f):
    try:
        req = urllib.request.urlopen('https://raw.githubusercontent.com/stevenblack/hosts/master/hosts')
        with open(f, "w") as file:
            file.write(req.read().decode("utf-8"))
    except:
        # we failed to pull (probably no internet), do nothing
        None

adblock_file = os.environ["HOME"] + '/.config/qutebrowser/adblock_internet.txt'

if os.path.exists(adblock_file):
    if (time.time() - os.path.getmtime(adblock_file)) > (60 * 60 * 24):
        pull_adblock(adblock_file)
else:
    pull_adblock(adblock_file)

blockfiles = [adblock_file,                                                 # internet block list
              os.environ["HOME"] + '/.config/qutebrowser/adblock.txt',      # distracting sites
              os.environ["HOME"] + '/.config/qutebrowser/adblock_temp.txt'] # local adhoc blocks

for f in blockfiles:
    if not os.path.exists(f):
        Path(f).touch()

# both = both host blocking and brave abp-style blocker
c.content.blocking.method = "both"
c.content.blocking.hosts.lists = ['file://' + f for f in blockfiles]
