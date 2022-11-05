/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx       = 1;        /* border pixel of windows */
static const unsigned int snap           = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;        /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayspacing = 2;        /* systray spacing */
static const int systraypinningfailfirst = 1;        /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray             = 1;     	 /* 0 means no systray */
static const unsigned int gappih         = 20;       /* horiz inner gap between windows */
static const unsigned int gappiv         = 10;       /* vert inner gap between windows */
static const unsigned int gappoh         = 10;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov         = 30;       /* vert outer gap between windows and screen edge */
static       int smartgaps               = 1;        /* 1 means no outer gap when there is only one window */
static const int showbar                 = 1;        /* 0 means no bar */
static const int topbar                  = 1;        /* 0 means bottom bar */
static const char *fonts[]               = { "monospace:size=9", "JoyPixels:pixelsize=10" };
static const char dmenufont[]            = "monospace:size=9";
static const char col_gray1[]            = "#222222";
static const char col_gray2[]            = "#4a0f85";
static const char col_gray3[]            = "#bbbbbb";
static const char col_gray4[]            = "#eeeeee";
static const char col_cyan[]             = "#bb73bb";
static const char *colors[][3]           = {
	/*                        fg         bg         border   */
	[SchemeNorm]      = { col_gray3, col_gray1,  col_gray2 },
	[SchemeSel]       = { col_gray4, col_cyan,   col_cyan  },
	[SchemeStatus]    = { col_gray4, "#260b2e",  "#000000"  }, // Statusbar right {text,background,not used but cannot be empty}
	[SchemeTagsSel]   = { "#d1d1d1", "#320a47",  "#000000"  }, // Tagbar left selected {text,background,not used but cannot be empty}
    [SchemeTagsNorm]  = { "#702d80", "#260b2e",  "#000000"  }, // Tagbar left unselected {text,background,not used but cannot be empty}
    [SchemeInfoSel]   = { col_gray4, "#000000",  "#000000"  }, // infobar middle  selected {text,background,not used but cannot be empty}
    [SchemeInfoNorm]  = { col_gray4, col_gray2,  "#000000"  }, // infobar middle  unselected {text,background,not used but cannot be empty}
};

static const char *const autostart[] = {
	"/home/vamp/.config/wm/bin/keyss", NULL,
    "feh", "--bg-fill", "/home/vamp/.config/wm/wp.png", NULL,
    "dwmblocks", NULL,
    "picom", NULL,
    "flameshot", NULL,
    "redshift", NULL,
    "emacs", "--daemon", NULL,
	NULL /* terminate */
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" };

static const char ptagf[] = "[%s] %s";	/* format of a tag label */
static const char etagf[] = "[%s]";	/* format of an empty tag */
static const int lcaselbl = 0;		/* 1 means make tag label lowercase */

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       0,            1,           -1 },
	{ "Firefox",  NULL,       NULL,       1 << 8,       0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

#define FORCE_VSPLIT 1  /* nrowgrid layout: force two clients to always split vertically */
#include "vanitygaps.c"

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "[M]",      monocle },
	{ "[@]",      spiral },
	{ "[\\]",     dwindle },
	{ "H[]",      deck },
	{ "TTT",      bstack },
	{ "===",      bstackhoriz },
	{ "HHH",      grid },
	{ "###",      nrowgrid },
	{ "---",      horizgrid },
	{ ":::",      gaplessgrid },
	{ "|M|",      centeredmaster },
	{ ">M>",      centeredfloatingmaster },
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ NULL,       NULL },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf", col_gray4, NULL };
static const char *roficmd[] = { "rofi", "-show", "combi", NULL };
static const char *termcmd[]  = { "alacritty", "-e", "/bin/zsh", NULL };
static const char scratchpadname[] = "scratchpad";
static const char *scratchpadcmd[] = { "st", "-t", scratchpadname, "-g", "69x13", "-e", "/bin/zsh", NULL };

#include "movestack.c"
#include <X11/XF86keysym.h>
static Key keys[] = {
	/* modifier                     key        function        argument */
    { MODKEY|ShiftMask,             XK_grave,  spawn,          SHCMD("/home/vamp/.config/wm/bin/lock-scr") },
	{ MODKEY,                       XK_d,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_d,      spawn,          {.v = roficmd } },
	{ MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY|ShiftMask,             XK_Return, togglescratch,  {.v = scratchpadcmd } },
	//{ MODKEY,                       XK_s,      togglescratch,  SHCMD("firefox") },

    { MODKEY,                       XK_w,      spawn,          SHCMD("firejail firefox-bin") },
    //{ MODKEY|ShiftMask,             XK_w,      spawn,          SHCMD("firejail librewolf") },
    //{ MODKEY,                       XK_t,      spawn,          SHCMD("qutebrowser") },
    //{ MODKEY|ShiftMask,             XK_t,      spawn,          SHCMD("google-chrome-stable") },

    { MODKEY,                       XK_t,      spawn,          SHCMD("emacsclient -c") },
    { MODKEY|ShiftMask,             XK_t,      spawn,          SHCMD("emacs") },

	//{ MODKEY,		                XK_m,	   spawn,		   SHCMD("dbus-run-session flatpak run com.sindresorhus.Caprine") },
	//{ MODKEY|ShiftMask,             XK_m,	   spawn,		   SHCMD("dbus-run-session flatpak run com.discordapp.Discord") },

    { MODKEY,                       XK_x,      spawn,          SHCMD("flameshot gui") },

	{ MODKEY,                       XK_f,      togglefullscr,  {0} },
	{ MODKEY|ShiftMask,             XK_f,      togglebar,      {0} },

	{ MODKEY,		                XK_v,	   spawn,		   SHCMD("virt-manager") },
	{ MODKEY|ShiftMask,		        XK_v,	   spawn,		   SHCMD("firejail virtualbox") },

	{ MODKEY,		                XK_k,	   spawn,		   SHCMD("alacritty -e newsboat") },

	{ MODKEY,			            XK_F11,	   spawn,          SHCMD("/home/vamp/.config/wm/bin/keylay-on; kill -43 $(pidof dwmblocks)") },
	{ MODKEY|ShiftMask,			    XK_F11,	   spawn,          SHCMD("/home/vamp/.config/wm/bin/keylay-off && /home/vamp/.config/wm/bin/keyss; kill -43 $(pidof dwmblocks)") },

	{ MODKEY,			            XK_F12,	   spawn,          SHCMD("/home/vamp/.config/wm/bin/keyss") },


	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_q,      killclient,     {0} },

	{ MODKEY,                       XK_space,  togglefloating, {0} },

    //-----------------------------------COLEMAK-DH----------------------------------------
    // Change focus window
	{ MODKEY,                       XK_n,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_e,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_l,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_h,      focusstack,     {.i = -1 } },
    // Change grid
	{ MODKEY,                       XK_u,      incnmaster,     {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_u,      incnmaster,     {.i = -1 } },
    // Resize window
	{ MODKEY|ShiftMask,             XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY|ShiftMask,             XK_l,      setmfact,       {.f = +0.05} },
    // Move window Master/Slave
	{ MODKEY|ShiftMask,             XK_n,      movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_e,      movestack,      {.i = -1 } },
    // Move floating window
	{ ControlMask|ShiftMask,        XK_n,      moveresize,     {.v = "0x 25y 0w 0h" } },
	{ ControlMask|ShiftMask,        XK_e,      moveresize,     {.v = "0x -25y 0w 0h" } },
	{ ControlMask|ShiftMask,        XK_l,      moveresize,     {.v = "25x 0y 0w 0h" } },
	{ ControlMask|ShiftMask,        XK_h,      moveresize,     {.v = "-25x 0y 0w 0h" } },
    // Resize flating window
	{ MODKEY|ControlMask|ShiftMask, XK_n,      moveresize,     {.v = "0x 0y 0w 25h" } },
	{ MODKEY|ControlMask|ShiftMask, XK_e,      moveresize,     {.v = "0x 0y 0w -25h" } },
	{ MODKEY|ControlMask|ShiftMask, XK_l,      moveresize,     {.v = "0x 0y 25w 0h" } },
	{ MODKEY|ControlMask|ShiftMask, XK_h,      moveresize,     {.v = "0x 0y -25w 0h" } },

    //-----------------------------------QWERTY-------------------------------------------
    //// Change focus window
	//{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	//{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	//{ MODKEY,                       XK_l,      focusstack,     {.i = +1 } },
	//{ MODKEY,                       XK_h,      focusstack,     {.i = -1 } },
    //// Change grid
	//{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	//{ MODKEY|ShiftMask,             XK_i,      incnmaster,     {.i = -1 } },
    //// Resize window
	//{ MODKEY|ShiftMask,             XK_h,      setmfact,       {.f = -0.05} },
	//{ MODKEY|ShiftMask,             XK_l,      setmfact,       {.f = +0.05} },
    //// Move window Master/Slave
	//{ MODKEY|ShiftMask,             XK_j,      movestack,      {.i = +1 } },
	//{ MODKEY|ShiftMask,             XK_k,      movestack,      {.i = -1 } },
    //// Move floating window
	//{ ControlMask|ShiftMask,        XK_j,      moveresize,     {.v = "0x 25y 0w 0h" } },
	//{ ControlMask|ShiftMask,        XK_k,      moveresize,     {.v = "0x -25y 0w 0h" } },
	//{ ControlMask|ShiftMask,        XK_l,      moveresize,     {.v = "25x 0y 0w 0h" } },
	//{ ControlMask|ShiftMask,        XK_h,      moveresize,     {.v = "-25x 0y 0w 0h" } },
    //// Resize flating window
	//{ MODKEY|ControlMask|ShiftMask, XK_j,      moveresize,     {.v = "0x 0y 0w 25h" } },
	//{ MODKEY|ControlMask|ShiftMask, XK_k,      moveresize,     {.v = "0x 0y 0w -25h" } },
	//{ MODKEY|ControlMask|ShiftMask, XK_l,      moveresize,     {.v = "0x 0y 25w 0h" } },
	//{ MODKEY|ControlMask|ShiftMask, XK_h,      moveresize,     {.v = "0x 0y -25w 0h" } },

	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	TAGKEYS(                        XK_0,                      9)
	{ MODKEY|ShiftMask,             XK_BackSpace,      quit,           {0} },

    { 0, XF86XK_AudioRaiseVolume,                spawn,         SHCMD("pactl set-sink-volume @DEFAULT_SINK@ +10%; kill -38 $(pidof dwmblocks)") },
    { 0, XF86XK_AudioLowerVolume,                spawn,         SHCMD("pactl set-sink-volume @DEFAULT_SINK@ -10%; kill -38 $(pidof dwmblocks)") },
    { 0, XF86XK_AudioMute,                       spawn,         SHCMD("pactl set-sink-mute @DEFAULT_SINK@ toggle; kill -38 $(pidof dwmblocks)") },
    { 0, XF86XK_AudioMicMute,                    spawn,         SHCMD("pactl set-source-mute @DEFAULT_SOURCE@ toggle; kill -38 $(pidof dwmblocks)") },

    { 0, XF86XK_AudioPlay,                       spawn,         SHCMD("mocp -G; kill -40 $(pidof dwmblocks)") },
    { 0, XF86XK_AudioNext,                       spawn,         SHCMD("mocp -f; kill -40 $(pidof dwmblocks)") },
    { 0, XF86XK_AudioPrev,                       spawn,         SHCMD("mocp -r; kill -40 $(pidof dwmblocks)") },

    { 0, XF86XK_MonBrightnessDown,               spawn,         SHCMD("light -U 5; kill -36 $(pidof dwmblocks)") },
    { 0, XF86XK_MonBrightnessUp,                 spawn,         SHCMD("light -A 10; kill -36 $(pidof dwmblocks)") },

	//{ MODKEY|ControlMask,           XK_Up,     moveresizeedge, {.v = "t"} },
	//{ MODKEY|ControlMask,           XK_Down,   moveresizeedge, {.v = "b"} },
	//{ MODKEY|ControlMask,           XK_Left,   moveresizeedge, {.v = "l"} },
	//{ MODKEY|ControlMask,           XK_Right,  moveresizeedge, {.v = "r"} },
	//{ MODKEY|ControlMask|ShiftMask, XK_Up,     moveresizeedge, {.v = "T"} },
	//{ MODKEY|ControlMask|ShiftMask, XK_Down,   moveresizeedge, {.v = "B"} },
	//{ MODKEY|ControlMask|ShiftMask, XK_Left,   moveresizeedge, {.v = "L"} },
	//{ MODKEY|ControlMask|ShiftMask, XK_Right,  moveresizeedge, {.v = "R"} },
	//{ MODKEY,                       XK_Return, zoom,           {0} },
	//{ MODKEY|Mod4Mask,              XK_u,      incrgaps,       {.i = +1 } },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_u,      incrgaps,       {.i = -1 } },
	//{ MODKEY|Mod4Mask,              XK_i,      incrigaps,      {.i = +1 } },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_i,      incrigaps,      {.i = -1 } },
	//{ MODKEY|Mod4Mask,              XK_o,      incrogaps,      {.i = +1 } },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_o,      incrogaps,      {.i = -1 } },
	//{ MODKEY|Mod4Mask,              XK_6,      incrihgaps,     {.i = +1 } },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_6,      incrihgaps,     {.i = -1 } },
	//{ MODKEY|Mod4Mask,              XK_7,      incrivgaps,     {.i = +1 } },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_7,      incrivgaps,     {.i = -1 } },
	//{ MODKEY|Mod4Mask,              XK_8,      incrohgaps,     {.i = +1 } },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_8,      incrohgaps,     {.i = -1 } },
	//{ MODKEY|Mod4Mask,              XK_9,      incrovgaps,     {.i = +1 } },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_9,      incrovgaps,     {.i = -1 } },
	//{ MODKEY|Mod4Mask,              XK_0,      togglegaps,     {0} },
	//{ MODKEY|Mod4Mask|ShiftMask,    XK_0,      defaultgaps,    {0} },
	//{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	//{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	//{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	//{ MODKEY,                       XK_space,  setlayout,      {0} },
	//{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	//{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	//{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	//{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	//{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	//{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

