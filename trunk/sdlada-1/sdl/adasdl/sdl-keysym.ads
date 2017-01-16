
-- ----------------------------------------------------------------- --
--                AdaSDL                                             --
--                Thin binding to Simple Direct Media Layer          --
--                Copyright (C) 2000-2012  A.M.F.Vargas              --
--                Antonio M. M. Ferreira Vargas                      --
--                Manhente - Barcelos - Portugal                     --
--                http://adasdl.sourceforge.net                      --
--                E-mail: amfvargas@gmail.com                        --
-- ----------------------------------------------------------------- --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-- ----------------------------------------------------------------- --

--  **************************************************************** --
--  This is an Ada binding to SDL ( Simple DirectMedia Layer from    --
--  Sam Lantinga - www.libsld.org )                                  --
--  **************************************************************** --
--  In order to help the Ada programmer, the comments in this file   --
--  are, in great extent, a direct copy of the original text in the  --
--  SDL header files.                                                --
--  **************************************************************** --

with Interfaces.C;

package SDL.Keysym is

   package C renames Interfaces.C;
      --  The keyboard syms have been cleverly chosen to map to ASCII


   type Key is new C.int;
      --  The keyboard syms have been cleverly chosen to map to ASCII
   K_UNKNOWN      : constant Key :=   0;
   K_FIRST        : constant Key :=   0;
   K_BACKSPACE    : constant Key :=   8;
   K_TAB          : constant Key :=   9;
   K_CLEAR        : constant Key :=  12;
   K_RETURN       : constant Key :=  13;
   K_PAUSE        : constant Key :=  19;
   K_ESCAPE       : constant Key :=  27;
   K_SPACE        : constant Key :=  32;
   K_EXCLAIM      : constant Key :=  33;
   K_QUOTEDBL     : constant Key :=  34;
   K_HASH         : constant Key :=  35;
   K_DOLLAR       : constant Key :=  36;
   K_AMPERSAND    : constant Key :=  38;
   K_QUOTE        : constant Key :=  39;
   K_LEFTPAREN    : constant Key :=  40;
   K_RIGHTPAREN   : constant Key :=  41;
   K_ASTERISK     : constant Key :=  42;
   K_PLUS         : constant Key :=  43;
   K_COMMA        : constant Key :=  44;
   K_MINUS        : constant Key :=  45;
   K_PERIOD       : constant Key :=  46;
   K_SLASH        : constant Key :=  47;
   K_0            : constant Key :=  48;
   K_1            : constant Key :=  49;
   K_2            : constant Key :=  50;
   K_3            : constant Key :=  51;
   K_4            : constant Key :=  52;
   K_5            : constant Key :=  53;
   K_6            : constant Key :=  54;
   K_7            : constant Key :=  55;
   K_8            : constant Key :=  56;
   K_9            : constant Key :=  57;
   K_COLON        : constant Key :=  58;
   K_SEMICOLON    : constant Key :=  59;
   K_LESS         : constant Key :=  60;
   K_EQUALS       : constant Key :=  61;
   K_GREATER      : constant Key :=  62;
   K_QUESTION     : constant Key :=  63;
   K_AT           : constant Key :=  64;

      --  Skip uppercase letters
   K_LEFTBRACKET  : constant Key :=  91;
   K_BACKSLASH    : constant Key :=  92;
   K_RIGHTBRACKET : constant Key :=  93;
   K_CARET        : constant Key :=  94;
   K_UNDERSCORE   : constant Key :=  95;
   K_BACKQUOTE    : constant Key :=  96;
   K_a            : constant Key :=  97;
   K_b            : constant Key :=  98;
   K_c            : constant Key :=  99;
   K_d            : constant Key := 100;
   K_e            : constant Key := 101;
   K_f            : constant Key := 102;
   K_g            : constant Key := 103;
   K_h            : constant Key := 104;
   K_i            : constant Key := 105;
   K_j            : constant Key := 106;
   K_k            : constant Key := 107;
   K_l            : constant Key := 108;
   K_m            : constant Key := 109;
   K_n            : constant Key := 110;
   K_o            : constant Key := 111;
   K_p            : constant Key := 112;
   K_q            : constant Key := 113;
   K_r            : constant Key := 114;
   K_s            : constant Key := 115;
   K_t            : constant Key := 116;
   K_u            : constant Key := 117;
   K_v            : constant Key := 118;
   K_w            : constant Key := 119;
   K_x            : constant Key := 120;
   K_y            : constant Key := 121;
   K_z            : constant Key := 122;
   K_DELETE       : constant Key := 127;

      --  End of ASCII mapped keysyms

      --  International keyboard syms
   K_WORLD_0      : constant Key := 160;      --  0xA0
   K_WORLD_1      : constant Key := 161;
   K_WORLD_2      : constant Key := 162;
   K_WORLD_3      : constant Key := 163;
   K_WORLD_4      : constant Key := 164;
   K_WORLD_5      : constant Key := 165;
   K_WORLD_6      : constant Key := 166;
   K_WORLD_7      : constant Key := 167;
   K_WORLD_8      : constant Key := 168;
   K_WORLD_9      : constant Key := 169;
   K_WORLD_10     : constant Key := 170;
   K_WORLD_11     : constant Key := 171;
   K_WORLD_12     : constant Key := 172;
   K_WORLD_13     : constant Key := 173;
   K_WORLD_14     : constant Key := 174;
   K_WORLD_15     : constant Key := 175;
   K_WORLD_16     : constant Key := 176;
   K_WORLD_17     : constant Key := 177;
   K_WORLD_18     : constant Key := 178;
   K_WORLD_19     : constant Key := 179;
   K_WORLD_20     : constant Key := 180;
   K_WORLD_21     : constant Key := 181;
   K_WORLD_22     : constant Key := 182;
   K_WORLD_23     : constant Key := 183;
   K_WORLD_24     : constant Key := 184;
   K_WORLD_25     : constant Key := 185;
   K_WORLD_26     : constant Key := 186;
   K_WORLD_27     : constant Key := 187;
   K_WORLD_28     : constant Key := 188;
   K_WORLD_29     : constant Key := 189;
   K_WORLD_30     : constant Key := 190;
   K_WORLD_31     : constant Key := 191;
   K_WORLD_32     : constant Key := 192;
   K_WORLD_33     : constant Key := 193;
   K_WORLD_34     : constant Key := 194;
   K_WORLD_35     : constant Key := 195;
   K_WORLD_36     : constant Key := 196;
   K_WORLD_37     : constant Key := 197;
   K_WORLD_38     : constant Key := 198;
   K_WORLD_39     : constant Key := 199;
   K_WORLD_40     : constant Key := 200;
   K_WORLD_41     : constant Key := 201;
   K_WORLD_42     : constant Key := 202;
   K_WORLD_43     : constant Key := 203;
   K_WORLD_44     : constant Key := 204;
   K_WORLD_45     : constant Key := 205;
   K_WORLD_46     : constant Key := 206;
   K_WORLD_47     : constant Key := 207;
   K_WORLD_48     : constant Key := 208;
   K_WORLD_49     : constant Key := 209;
   K_WORLD_50     : constant Key := 210;
   K_WORLD_51     : constant Key := 211;
   K_WORLD_52     : constant Key := 212;
   K_WORLD_53     : constant Key := 213;
   K_WORLD_54     : constant Key := 214;
   K_WORLD_55     : constant Key := 215;
   K_WORLD_56     : constant Key := 216;
   K_WORLD_57     : constant Key := 217;
   K_WORLD_58     : constant Key := 218;
   K_WORLD_59     : constant Key := 219;
   K_WORLD_60     : constant Key := 220;
   K_WORLD_61     : constant Key := 221;
   K_WORLD_62     : constant Key := 222;
   K_WORLD_63     : constant Key := 223;
   K_WORLD_64     : constant Key := 224;
   K_WORLD_65     : constant Key := 225;
   K_WORLD_66     : constant Key := 226;
   K_WORLD_67     : constant Key := 227;
   K_WORLD_68     : constant Key := 228;
   K_WORLD_69     : constant Key := 229;
   K_WORLD_70     : constant Key := 230;
   K_WORLD_71     : constant Key := 231;
   K_WORLD_72     : constant Key := 232;
   K_WORLD_73     : constant Key := 233;
   K_WORLD_74     : constant Key := 234;
   K_WORLD_75     : constant Key := 235;
   K_WORLD_76     : constant Key := 236;
   K_WORLD_77     : constant Key := 237;
   K_WORLD_78     : constant Key := 238;
   K_WORLD_79     : constant Key := 239;
   K_WORLD_80     : constant Key := 240;
   K_WORLD_81     : constant Key := 241;
   K_WORLD_82     : constant Key := 242;
   K_WORLD_83     : constant Key := 243;
   K_WORLD_84     : constant Key := 244;
   K_WORLD_85     : constant Key := 245;
   K_WORLD_86     : constant Key := 246;
   K_WORLD_87     : constant Key := 247;
   K_WORLD_88     : constant Key := 248;
   K_WORLD_89     : constant Key := 249;
   K_WORLD_90     : constant Key := 250;
   K_WORLD_91     : constant Key := 251;
   K_WORLD_92     : constant Key := 252;
   K_WORLD_93     : constant Key := 253;
   K_WORLD_94     : constant Key := 254;
   K_WORLD_95     : constant Key := 255;      --  0xFF

      --  Numeric keypad
   K_KP0          : constant Key := 256;
   K_KP1          : constant Key := 257;
   K_KP2          : constant Key := 258;
   K_KP3          : constant Key := 259;
   K_KP4          : constant Key := 260;
   K_KP5          : constant Key := 261;
   K_KP6          : constant Key := 262;
   K_KP7          : constant Key := 263;
   K_KP8          : constant Key := 264;
   K_KP9          : constant Key := 265;
   K_KP_PERIOD    : constant Key := 266;
   K_KP_DIVIDE    : constant Key := 267;
   K_KP_MULTIPLY  : constant Key := 268;
   K_KP_MINUS     : constant Key := 269;
   K_KP_PLUS      : constant Key := 270;
   K_KP_ENTER     : constant Key := 271;
   K_KP_EQUALS    : constant Key := 272;

      --  Arrows + Home/End pad
   K_UP           : constant Key := 273;
   K_DOWN         : constant Key := 274;
   K_RIGHT        : constant Key := 275;
   K_LEFT         : constant Key := 276;
   K_INSERT       : constant Key := 277;
   K_HOME         : constant Key := 278;
   K_END          : constant Key := 279;
   K_PAGEUP       : constant Key := 280;
   K_PAGEDOWN     : constant Key := 281;

      --  Function keys
   K_F1           : constant Key := 282;
   K_F2           : constant Key := 283;
   K_F3           : constant Key := 284;
   K_F4           : constant Key := 285;
   K_F5           : constant Key := 286;
   K_F6           : constant Key := 287;
   K_F7           : constant Key := 288;
   K_F8           : constant Key := 289;
   K_F9           : constant Key := 290;
   K_F10          : constant Key := 291;
   K_F11          : constant Key := 292;
   K_F12          : constant Key := 293;
   K_F13          : constant Key := 294;
   K_F14          : constant Key := 295;
   K_F15          : constant Key := 296;

      --  Key state modifier keys
   K_NUMLOCK      : constant Key := 300;
   K_CAPSLOCK     : constant Key := 301;
   K_SCROLLOCK    : constant Key := 302;
   K_RSHIFT       : constant Key := 303;
   K_LSHIFT       : constant Key := 304;
   K_RCTRL        : constant Key := 305;
   K_LCTRL        : constant Key := 306;
   K_RALT         : constant Key := 307;
   K_LALT         : constant Key := 308;
   K_RMETA        : constant Key := 309;
   K_LMETA        : constant Key := 310;
   K_LSUPER       : constant Key := 311;      --  Left "Windows" key
   K_RSUPER       : constant Key := 312;      --  Right "Windows" key
   K_MODE         : constant Key := 313;      --  "Alt Gr" key
   K_COMPOSE      : constant Key := 314;      --  Multi-key compose key

      --  Miscellaneous function keys
   K_HELP         : constant Key := 315;
   K_PRINT        : constant Key := 316;
   K_SYSREQ       : constant Key := 317;
   K_BREAK        : constant Key := 318;
   K_MENU         : constant Key := 319;
   K_POWER        : constant Key := 320;      --  Power Macintosh power key */
   K_EURO         : constant Key := 321;      --  Some european keyboards */

      --  Add any other keys here

   K_LAST         : constant Key := 322;


   --  Enumeration of valid key mods (possibly OR'd together)
   type SDLMod is mod 2**32;
   pragma Convention (C, SDLMod);

   KMOD_NONE     : constant SDLMod := 16#0000#;
   KMOD_LSHIFT   : constant SDLMod := 16#0001#;
   KMOD_RSHIFT   : constant SDLMod := 16#0002#;
   KMOD_LCTRL    : constant SDLMod := 16#0040#;
   KMOD_RCTRL    : constant SDLMod := 16#0080#;
   KMOD_LALT     : constant SDLMod := 16#0100#;
   KMOD_RALT     : constant SDLMod := 16#0200#;
   KMOD_LMETA    : constant SDLMod := 16#0400#;
   KMOD_RMETA    : constant SDLMod := 16#0800#;
   KMOD_NUM      : constant SDLMod := 16#1000#;
   KMOD_CAPS     : constant SDLMod := 16#2000#;
   KMOD_MODE     : constant SDLMod := 16#4000#;
   KMOD_RESERVED : constant SDLMod := 16#8000#;

   KMOD_CTRL     : constant SDLMod := (KMOD_LCTRL or KMOD_RCTRL);
   KMOD_SHIFT    : constant SDLMod := (KMOD_LSHIFT or KMOD_RSHIFT);
   KMOD_ALT      : constant SDLMod := (KMOD_LALT or KMOD_RALT);
   KMOD_META     : constant SDLMod := (KMOD_LMETA or KMOD_RMETA);

end SDL.Keysym;
