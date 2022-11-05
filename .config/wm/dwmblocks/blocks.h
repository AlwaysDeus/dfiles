//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/		              /*Update Interval*/	/*Update Signal*/
    {"",    "$HOME/.config/wm/bin/camera",          6000,   8},

    {"",    "$HOME/.config/wm/bin/progress-bar",    60,   10},

    {"",    "$HOME/.config/wm/bin/wifi",            10,     7},

    {"",    "$HOME/.config/wm/bin/music-bar",       10,     6},

    {"💡",	"light | awk -F '.' '{print $1}'",      0,      2},

    {"",	"$HOME/.config/wm/bin/battery",         60,     3},

    {"",    "$HOME/.config/wm/bin/keylay-bar",      0,      9},

	{"",	"$HOME/.config/wm/bin/volume",	        0,      4},

	{"",    "date '+%b %d (%a) %I:%M%p'",			60,     0},

	//{"",    " ",                                    360,  1}
};

// kill -44 $(pidof dwmblocks)  add 34 to your typical signal number.
//
//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = " | ";
static unsigned int delimLen = 5;

