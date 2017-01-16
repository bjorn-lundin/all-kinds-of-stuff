
--- ----------------------------------------------------------------- --
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

with System;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with SDL.Types; use SDL.Types;
with SDL.Mutex;
with SDL.RWops;
use type Interfaces.C.int;
package SDL.Video is

   package CS  renames Interfaces.C.Strings;
   package US  renames Ada.Strings.Unbounded;

   --  Transparency definitions: These define
   --  alpha as the opacity of a surface.
   ALPHA_OPAQUE : constant := 255;
   ALPHA_TRANSPARENT : constant := 0;

   --  Useful data types
   type Rect is
     record
        x, y : Sint16;
        w, h : Uint16;
     end record;
   pragma Convention (C, Rect);

   type Rect_ptr is access all Rect;
   pragma Convention (C, Rect_ptr);
   type Rect_ptr_ptr is access all Rect_ptr;
   pragma Convention (C, Rect_ptr_ptr);
   --  type Rects_Array is array (Natural range <>) of aliased Rect;
   type Rects_Array is array (C.unsigned range <>) of aliased Rect;
   type Rects_Array_Access is access all Rects_Array;

   type Color is
      record
         r,
         g,
         b,
         unused : Uint8;
      end record;
   pragma Convention (C, Color);

   type Color_ptr is access all Color;
   pragma Convention (C, Color_ptr);

   Null_Color : Color := (0, 0, 0, 123);
   type Colors_Array is
      array (C.size_t range <>)
         of aliased Color;
   package Color_PtrOps is new C.Pointers (
      Index              => C.size_t,
      Element            => Color,
      Element_Array      => Colors_Array,
      Default_Terminator => Null_Color);
   --  You must do a "use  Color_PtrOps"
   --  in your code

   type Palette is
      record
         ncolors : C.int;
         colors  : Color_ptr;
      end record;
   pragma Convention (C, Palette);

   type Palette_ptr is  access Palette;
   pragma Convention (C, Palette_ptr);

   --  Everything in the pixel format structure
   --  is read-only. In Ada this can be controled
   --  using "access constant".
   type PixelFormat is
      record
         palette : Palette_ptr;
         BitsPerPixel,
         BytesPerPixel,
         Rloss,
         Gloss,
         Bloss,
         Aloss,
         Rshift,
         Gshift,
         Bshift,
         Ashift : Uint8;
         Rmask,
         Gmask,
         Bmask,
         Amask : Uint32;

         --  RGB color key information
         colorkey : Uint32;

         --  Alpha value information (per-surface alpha)
         alpha : Uint8;
      end record;
   pragma Convention (C, PixelFormat);

   type PixelFormat_ptr is access constant PixelFormat;
   pragma Convention (C, PixelFormat_ptr);

   type private_hwdata_ptr is new System.Address;

   type BlitMap_ptr is new System.Address;

   type Surface_Flags is mod 2 ** 32;
   for Surface_Flags'Size use 32;

   --  This structure should be treated as read-only
   type Surface is
      record
         flags : Surface_Flags;     --  Read-only
         format : PixelFormat_ptr;  --  Read-only
         w, h : C.int;              --  Read-only
         pitch : Uint16;            --  Read-only
         pixels : System.Address;   --  Read-write
         offset : C.int;            --  Private

         --  Hardware-specific surface info
         hwdata : private_hwdata_ptr;

         --  Clipping information
         clip_rect : Rect;          --  Read-only
         unused1 : Uint32;          --  For binary compatibility

         --  Allow recursive locks
         locked : Uint32;           --  Private

         --  Info for fast blit mapping to other surfaces
         map : BlitMap_ptr; --  Private

         --  Format version, bumped at every change to
         --  invalidate blit maps.
         format_version : C.unsigned; -- Private

         --  Reference count -- used when freeing surface
         refcount : C.int;          -- Read-mostly
      end record;
   pragma Convention (C, Surface);

   type Surface_ptr is access all Surface;
   pragma Convention (C, Surface_ptr);

   null_Surface_ptr : constant Surface_ptr := null;

   --  These are the currently supported flags for
   --  the SDL_surface.

   --  ----------------------------------------------
   --  Available for CreateRGBSurface or SetVideoMode.

   --  Surface is in system memory
   SWSURFACE   : constant Surface_Flags := 16#00000000#;

   --  Surface is in video memory
   HWSURFACE   : constant Surface_Flags := 16#00000001#;

   --  Use asynchronous blits if possible
   ASYNCBLIT   : constant Surface_Flags := 16#00000004#;

   --  ----------------------------------------------
   --  Available for SetVideoMode

   --  Allow any video depth/pixel-format
   ANYFORMAT   : constant Surface_Flags := 16#10000000#;

   --  Surface has exclusive palette
   HWPALETTE   : constant Surface_Flags := 16#20000000#;

   --  Set up double-buffered video mode
   DOUBLEBUF   : constant Surface_Flags := 16#40000000#;

   --  Surface is a full screen display
   FULLSCREEN  : constant Surface_Flags := 16#80000000#;

   --  Create an OpenGL rendering context
   OPENGL      : constant Surface_Flags := 16#00000002#;

   --  Create an OpenGL rendering context and use if
   --  for blitting
   OPENGLBLIT  : constant Surface_Flags := 16#0000000A#;

   --  This video mode may be resized
   RESIZABLE   : constant Surface_Flags := 16#00000010#;

   --  No window caption or edge frame
   NOFRAME     : constant Surface_Flags := 16#00000020#;

   --  -----------------------------------------------
   --  Use internally (read-only

   --  Blit uses hardware acceleration
   HWACCEL     : constant Surface_Flags := 16#00000100#;

   --  Blit uses a source color key
   SRCCOLORKEY : constant Surface_Flags := 16#00001000#;

   --  Private flag
   RLEACCELOK  : constant Surface_Flags := 16#00002000#;

   --  Surface is RLE encoded
   RLEACCEL    : constant Surface_Flags := 16#00004000#;

   --  Blit uses source alpha blending
   SRCALPHA    : constant Surface_Flags := 16#00010000#;

   --  Surface uses preallocated memory
   PREALLOC    : constant Surface_Flags := 16#01000000#;

   --  Evaluates to true if the surface needs to be locked
   --  before access.
   function MUSTLOCK (surface : Surface_ptr) return Boolean;
   pragma Inline (MUSTLOCK);

   --  Useful for determining the video hardware capabilities

   type VideoInfo is
      record
         hw_available : bits1;  --  Flag: Can you create hardware surfaces?
         wm_available : bits1;  --  Flag: Can you talk to a window manager?
         UnusedBits1  : bits6;
         UnusedBits2  : bits1;
         blit_hw      : bits1;  --  Flag: Accelerated blits HW --> HW
         blit_hw_CC   : bits1;  --  Flag: Accelerated blits with Colorkey
         blit_hw_A    : bits1;  --  Flag: Accelerated blits with Alpha
         blit_sw      : bits1;  --  Flag: Accelerated blits SW --> HW
         blit_sw_CC   : bits1;  --  Flag: Accelerated blits with Colorkey
         blit_sw_A    : bits1;  --  Flag: Accelerated blits with Alpha
         blit_fill    : bits1;  --  Flag: Accelerated color fill
         UnusedBits3  : bits16;
         video_mem    : Uint32; --  The total amount of video memory (in K)
         --  Value: The format of the video surface
         vfmt         : PixelFormat_ptr;
         current_w    : C.Int;
         current_h    : C.Int;
      end record;

   NBits : constant := System.Word_Size;
   NBytes : constant := System.Word_Size/8;

   NBitsCInt : constant := C.int'Size;
   NBytesCInt : constant := C.int'Size/8;

   for VideoInfo use
      record
         hw_available at 0 range 0 .. 0;
         wm_available at 0 range 1 .. 1;
         UnusedBits1  at 0 range 2 .. 7;
         UnusedBits2  at 1 range 0 .. 0;
         blit_hw      at 1 range 1 .. 1;
         blit_hw_CC   at 1 range 2 .. 2;
         blit_hw_A    at 1 range 3 .. 3;
         blit_sw      at 1 range 4 .. 4;
         blit_sw_CC   at 1 range 5 .. 5;
         blit_sw_A    at 1 range 6 .. 6;
         blit_fill    at 1 range 7 .. 7;
         UnusedBits3  at 2 range 0 .. 15;
         video_mem    at 4 range 0 .. 31;
         -- vfmt         at 8 range 0 .. 31;
         vfmt         at 4+4 range 0..NBits-1;
         current_w    at 4+4+NBytes range 0 .. NBitsCInt-1;
         current_h    at 4+4+NBytes + NBytesCInt range 0 .. NBitsCInt-1;
      end record;

   pragma Convention (C, VideoInfo);

   type VideoInfo_ptr is access all VideoInfo;
   pragma Convention (C, VideoInfo_ptr);

   type VideoInfo_ConstPtr is access constant VideoInfo;
   pragma Convention (C, VideoInfo_ConstPtr);

   --  The most common video ovelay formats.
   --  For an explanation of this pixel formats, see:
   --      http://www.webartz.com/fourcc/indexyuv.htm

   --  For information on the relationship between color spaces, see
   --  http://www.neuro.sfc.keio.ac.jp/~aly/polygon/info/color-space-faq.html

   --  Planar mode: Y + V + U (3 planes)
   YV12_OVERLAY : constant := 16#32315659#;
   --  Planar mode: Y + U + V (3 planes)
   IYUV_OVERLAY : constant := 16#56555949#;
   --  Packed mode: Y0 + U0 + Y1 + V0 (1 plane)
   YUY2_OVERLAY : constant := 16#32595559#;
   --  Packed mode: U0 + Y0 + V0 + Y1 (1 plane)
   UYVY_OVERLAY : constant := 16#59565955#;
   --  Packed mode: Y0 + V0 + Y1 + U0 (1 plane)
   YVYU_OVERLAY : constant := 16#55595659#;

   type private_yuvhwfuncs_ptr is new System.Address;
   type private_yuvhwdata_ptr is new System.Address;

   --  The YUV hardware video overlay
   type Overlay is
      record
         format : Uint32;          --  Read-only
         h, w :  C.int;            --  Read-only
         planes : C.int;           --  Read-only
         pitches : Uint16_ptr;     --  Read-only
         pixels : Uint8_ptr_ptr;   --  Read-write

         --  Hardware-specific surface info
         hwfuncs : private_yuvhwfuncs_ptr;
         hwdata : private_yuvhwdata_ptr;

         --  Special flags
         hw_overlay : bits1;
         UnusedBits : bits31;
      end record;


   for Overlay use
      record
         format      at 0*4 range 0 .. 31;
         h           at 1*4 range 0 .. 31;
         w           at 2*4 range 0 .. 31;
         planes      at 3*4 range 0 .. 31;
         pitches     at 4*4 range 0 .. (NBits-1);
         pixels      at 4*4 + 1*NBytes range 0 .. (NBits-1);
         hwfuncs     at 4*4 + 2*NBytes range 0 .. (NBits-1);
         hwdata      at 4*4 + 3*NBytes range 0 .. (NBits-1);
         hw_overlay  at 4*4 + 3*NBytes + 8 range 0 ..  0;
         UnusedBits  at 4*4 + 3*NBytes + 8 range 1 .. 31;
      end record;
   pragma Convention (C, Overlay);

   type Overlay_ptr is access all Overlay;
   pragma Convention (C, Overlay_ptr);

   type GLattr is new C.int;
   GL_RED_SIZE          : constant GLattr :=  0;
   GL_GREEN_SIZE        : constant GLattr :=  1;
   GL_BLUE_SIZE         : constant GLattr :=  2;
   GL_ALPHA_SIZE        : constant GLattr :=  3;
   GL_BUFFER_SIZE       : constant GLattr :=  4;
   GL_DOUBLEBUFFER      : constant GLattr :=  5;
   GL_DEPTH_SIZE        : constant GLattr :=  6;
   GL_STENCIL_SIZE      : constant GLattr :=  7;
   GL_ACCUM_RED_SIZE    : constant GLattr :=  8;
   GL_ACCUM_GREEN_SIZE  : constant GLattr :=  9;
   GL_ACCUM_BLUE_SIZE   : constant GLattr := 10;
   GL_ACCUM_ALPHA_SIZE  : constant GLattr := 11;


   LOGPAL : constant := 16#01#;
   PHYSPAL : constant := 16#02#;

   ---------------------------
   --  Function prototypes  --
   ---------------------------

   --  These functions are used internally, and should
   --  not be used unless you have a specific need to specify
   --  the video driver you want to use.
   --
   --  Binding note: I will not make this functions private
   --  because I wish to make them available just like the
   --  original C interface.
   --
   --  VideoInit initializes the video  subsystem --
   --  sets up a connection to the window manager, etc, and
   --  determines the current video mode and pixel format,
   --  but does not initialize a window or graphics mode.
   --  Note that event handling is activated by this routine.
   --
   --  If you use both sound and video in you application, you
   --  need to call Init before opening the sound device,
   --  otherwise under Win32 DirectX, you won't be able to set
   --  the full-screen display modes.

   function VideoInit (
      namebuf : C.Strings.chars_ptr;
      maxlen : C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, VideoInit, "SDL_VideoInit");


   procedure VideoQuit;
   pragma Import (C, VideoQuit, "SDL_VideoQuit");

   --  This function fills the given character buffer with the
   --  name of the video driver, and returns a pointer to it
   --  if the video driver has been initialized. It returns NULL
   --  if no driver has been initialized.
   function VideoDriverName (
      namebuf : C.Strings.chars_ptr;
      maxlen  : C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, VideoDriverName, "SDL_VideoDriverName");

   --  This function returns a pointer to the current display
   --  surface. If SDL is doing format conversion on the display
   --  surface, this function returns the publicly visible surface,
   --  not the real video surface.
   function GetVideoSurface return Surface_ptr;
   pragma Import (C, GetVideoSurface, "SDL_GetVideoSurface");

   --  This function returns a Read-only pointer to information
   --  about the video hardware. If this is called before SetVideoMode,
   --  the 'vfmt' member of the returned structure will contain the
   --  pixel format of the "best" video mode.
   function GetVideoInfo return VideoInfo_ConstPtr;
   pragma Import (C, GetVideoInfo, "SDL_GetVideoInfo");

   --  Check to see if a particular video mode is supported.
   --  It returns 0 if the requested video mode is not supported
   --  under any bit depth, or retuns the bits-per-pixel of the
   --  closest available mode with the given width and height. If
   --  this bits-per-pixel is different from the one used when
   --  setting the video mode, SetVideoMode will succeed, but
   --  will emulate the requested bits-per-pixel with a shadow
   --  surface.
   --
   --  The arguments to VideoModeOK are the same ones you would
   --  pass to SetVideoMode.
   function VideoModeOK (
      width  : C.int;
      height : C.int;
      bpp    : C.int;
      --  flags  : Uint32)
      flags  : Surface_Flags)
      return C.int;
   pragma Import (C, VideoModeOK, "SDL_VideoModeOK");

   --  Return a pointer to an array of available screen dimensions
   --  for the given format and video flags, sorted largest to
   --  smallest. Returns NULL if there are no dimensions available
   --  for a particular format, or Rect_ptr_ptr(-1) if any dimension
   --  is okay for the given format.
   --
   --  If 'format' is NULL, the mode list will be the format given
   --  by GetVideoInfo.all.vfmt
   function ListModes (
      format : PixelFormat_ptr;
      flags  : Surface_Flags)
      return Rect_ptr_ptr;
   pragma Import (C, ListModes, "SDL_ListModes");

   --  Necessary to verify if ListModes returns
   --  a non-pointer value like 0 or -1.
   function RectPP_To_Int is new
     Ada.Unchecked_Conversion (Rect_ptr_ptr, C.int);

   --  Necessary to manipulate C pointer from Ada
   type Rect_ptr_Array is array (C.size_t range <>)
     of aliased Rect_ptr;
   package Rect_ptr_PtrOps is new C.Pointers (
      Index => C.size_t,
      Element => Rect_ptr,
      Element_Array => Rect_ptr_Array,
      Default_Terminator  => null);
   --  You must do a --> use Rect_ptr_Ptrs; in your code.

   --  The limit of 20 is arbitrary
   --  The array scan should stop before 20 when a null is found.
   --  type Modes_Array is array (0 .. 19) of Rect_ptr;
   --  function ListModes (
   --     format : PixelFormat_ptr;
   --     flags  : Surface_Flags)
   --     return Modes_Array;
   --  pragma Import (C, ListModes, "SDL_ListModes");

   --  Set up a video mode with the specified width, height and
   --  and bits-per-pixel.
   --
   --  If 'bpp' is 0, it is treated as the current display
   --  bits-per-pixel.
   --
   --  If ANYFORMAT is set in 'flags', the SDL library will
   --  try to set the requestd bits-per-pixel, but will return
   --  whatever video pixel format is available. The default is
   --  to emulate the requested pixel format if it is not natively
   --  available.
   --
   --  If HWSURFACE is set in 'flags', the video surface will be
   --  placed in video memory, if possible, and you may have to
   --  call LockSurface in order to access the raw buffer. Otherwise
   --  the video surface will be created in the system memory.
   --
   --  If ASYNCBLIT is set in 'flags', SDL will try to perform
   --  rectangle updates asynchronously, but you must always lock
   --  before accessing pixels. SDL will wait for updates to
   --  complete before returning from the lock.
   --
   --  If HWPALETTE is set in 'flags', the SDL library will
   --  garantee that the colors set by SetColors will be the
   --  colors you get. Otherwise, in 8-bit mode, SetColors my
   --  not be able to set all of the colors exactly the way
   --  they are requested, and you should look at the video
   --  surface structure to determine the actual palette. If
   --  SDL cannot garantee that the colors you request can be
   --  set, i.e. if the color map is shared, then the video
   --  surface my be created under emulation in system memory,
   --  overriding the HWSURFACE flag.
   --
   --  If FULLSCREEN is set in 'flags', the SDL library will
   --  try to set a fullscreen video mode. The default is to
   --  create a windowed mode if the current graphics system
   --  has a window manager.
   --  If the SDL library is able to set a fullscreen mode,
   --  this flag will be set in the surface that is returned.
   --
   --  If DOUBLEBUF is set in 'flags', the SDL library will
   --  try to set up two surfaces in video memory and swap
   --  between them when you call Flip. This is usually slower
   --  than the normal single-buffering scheme, but prevents
   --  "tearing" artifacts caused by modifying video memory
   --  while the monitor is refreshing. It should only be used
   --  by applications that redraw the entire screen on every
   --  update.
   --
   --  This function returns the video buffer surface, or NULL
   --  if it fails.
   function SetVideoMode (
      width  : C.int;
      height : C.int;
      bpp    : C.int;
      flags  : Surface_Flags)
      return Surface_ptr;
   pragma Import (C, SetVideoMode, "SDL_SetVideoMode");

   --  Makes sure the given list of rectangles is updated on the
   --  given screen. If 'x','y','w' and 'h' are all 0, UpdateRect
   --  will update the entire screen.
   --  These functions  should  not be called while 'screen' is locked.
   procedure UpdateRects (
      screen    : Surface_ptr;
      numrects  : C.int;
      rects     : Rect_ptr);
   procedure UpdateRects (
      screen    : Surface_ptr;
      numrects  : C.int;
      rects     : Rects_Array);
   pragma Import (C, UpdateRects, "SDL_UpdateRects");

   --  Makes sure the given area is updated on the given screen.
   --  (In other words, it makes sure any changes to the given area
   --  of the screen are made visible)
   --  The rectangle must be confined within the screen boundaries
   --  (no clipping is done).
   --  If 'x', 'y', 'w' and 'h' are all 0, UpdateRect will update
   --  the entire screen.
   --  This function should not be called while 'screen' is locked (LockSurface).
   procedure UpdateRect (
      screen    : Surface_ptr;
      x         : Sint32;
      y         : Sint32;
      w         : Uint32;
      h         : Uint32);
   pragma Import (C, UpdateRect, "SDL_UpdateRect");

   --  Makes sure the given area is updated on the given screen.
   --  (In other words, it makes sure any changes to the given area
   --  of the screen are made visible)
   --  The rectangle must be confined within the screen boundaries
   --  (no clipping is done).
   --  If rect is (0,0,0,0), UpdateRect will update the entire screen.
   --  This function should not be called while 'screen' is locked (LockSurface).
   procedure Update_Rect (
      screen   : Surface_ptr;
      the_rect : Rect);
   pragma Inline (Update_Rect);

   --  On hardware that supports double-buffering, this function sets up a flip
   --  and returns.  The hardware will wait for vertical retrace, and then swap
   --  video buffers before the next video surface blit or lock will return.
   --  On hardware that doesn not support double-buffering, this is equivalent
   --  to calling UpdateRect (screen, 0, 0, 0, 0);
   --  The DOUBLEBUF flag must have been passed to SetVideoMode when
   --  setting the video mode for this function to perform hardware flipping.
   --  This function returns 0 if successful, or -1 if there was an error.
   function Flip (screen : Surface_ptr) return C.int;
   procedure Flip (screen : Surface_ptr);
   pragma Import (C, Flip, "SDL_Flip");

   --  Set the gamma correction for each of the color channels.
   --  The gamma values range (approximately) between 0.1 and 10.0
   --
   --  If this function isn't supported directly by the hardware, it will
   --  be emulated using gamma ramps, if available.  If successful, this
   --  function returns 0, otherwise it returns -1.
   function SetGamma (
      red   : C.C_float;
      green : C.C_float;
      blue  : C.C_float)
      return C.int;

   procedure SetGamma (
      red   : C.C_float;
      green : C.C_float;
      blue  : C.C_float);

   pragma Import (C, SetGamma, "SDL_SetGamma");

   --  Set the gamma translation table for the red, green, and blue channels
   --  of the video hardware.  Each table is an array of 256 16-bit quantities,
   --  representing a mapping between the input and output for that channel.
   --  The input is the index into the array, and the output is the 16-bit
   --  gamma value at that index, scaled to the output color precision.
   --
   --  You may pass NULL for any of the channels to leave it unchanged.
   --  If the call succeeds, it will return 0.  If the display driver or
   --  hardware does not support gamma translation, or otherwise fails,
   --  this function will return -1.
   function SetGammaRamp (
      red   : Uint16_ptr;
      green : Uint16_ptr;
      blue  : Uint16_ptr)
      return C.int;

   type ramp_Array is array (Natural range 0 .. 255) of aliased Uint16;

   function SetGammaRamp (
      red   : ramp_Array;
      green : ramp_Array;
      blue  : ramp_Array)
      return C.int;

   pragma Import (C, SetGammaRamp, "SDL_SetGammaRamp");

   --  Retrieve the current values of the gamma translation tables.
   --
   --  You must pass in valid pointers to arrays of 256 8-bit quantities.
   --  Any of the pointers may be NULL to ignore that channel.
   --  If the call succeeds, it will return 0.  If the display driver or
   --  hardware does not support gamma translation, or otherwise fails,
   --  this function will return -1.
   function GetGammaRamp (
      red   : Uint16_ptr;
      green : Uint16_ptr;
      blue  : Uint16_ptr)
      return C.int;
   pragma Import (C, GetGammaRamp, "SDL_GetGammaRamp");

   --  Sets a portion of the colormap for the given 8-bit surface.  If
   --  'surface' is not a palettized surface, this function does nothing,
   --  returning 0. If all of the colors were set as passed to SetColors,
   --  it will return 1.  If not all the color entries were set exactly as
   --  given, it will return 0, and you should look at the surface palette to
   --  determine the actual color palette.
   --
   --  When 'surface' is the surface associated with the current display, the
   --  display colormap will be updated with the requested colors.  If
   --  HWPALETTE was set in SetVideoMode flags, SetColors
   --  will always return 1, and the palette is guaranteed to be set the way
   --  you desire, even if the window colormap has to be warped or run under
   --  emulation.
   function SetColors (
      surface    : Surface_ptr;
      colors     : Color_ptr;
      firstcolor : C.int;
      ncolor     : C.int)
      return C.int;

   procedure SetColors (
      surface    : Surface_ptr;
      colors     : Color_ptr;
      firstcolor : C.int;
      ncolor     : C.int);

   function SetColors (
      surface    : Surface_ptr;
      colors     : Colors_Array;
      firstcolor : C.int;
      ncolor     : C.int)
      return C.int;

   procedure SetColors (
      surface    : Surface_ptr;
      colors     : Colors_Array;
      firstcolor : C.int;
      ncolor     : C.int);

   pragma Import (C, SetColors, "SDL_SetColors");

   function Set_Colors (
      surface    : Surface_ptr;
      colors     : Colors_Array)
      return C.int;

   procedure Set_Colors (
      surface    : Surface_ptr;
      colors     : Colors_Array);

   --  Sets a portion of the colormap for a given 8-bit surface.
   --  'flags' is one or both of:
   --  LOGPAL  -- set logical palette, which controls how blits are mapped
   --                 to/from the surface,
   --  PHYSPAL -- set physical palette, which controls how pixels look on
   --                 the screen
   --  Only screens have physical palettes. Separate change of physical/logical
   --  palettes is only possible if the screen has HWPALETTE set.
   --
   --  The return value is 1 if all colours could be set as requested, and 0
   --  otherwise.
   --
   --  SetColors is equivalent to calling this function with
   --      flags = (LOGPAL or PHYSPAL).
   function SetPalette (
      surface    : Surface_ptr;
      flags      : C.int;
      Colors     : Color_ptr;
      firstcolor : C.int;
      ncolors    : C.int)
      return C.int;

   function SetPalette (
      surface    : Surface_ptr;
      flags      : C.int;
      Colors     : Colors_Array;
      firstcolor : C.int;
      ncolors    : C.int)
      return C.int;

   procedure SetPalette (
      surface    : Surface_ptr;
      flags      : C.int;
      Colors     : Color_ptr;
      firstcolor : C.int;
      ncolors    : C.int);

   procedure SetPalette (
      surface    : Surface_ptr;
      flags      : C.int;
      Colors     : Colors_Array;
      firstcolor : C.int;
      ncolors    : C.int);

   pragma Import (C, SetPalette, "SDL_SetPalette");


   --  Maps an RGB triple to an opaque pixel value for a given pixel format
   function MapRGB (
      format : PixelFormat_ptr;
      r      : Uint8;
      g      : Uint8;
      b      : Uint8)
      return Uint32;
   pragma Import (C, MapRGB, "SDL_MapRGB");


   --  Maps an RGBA quadruple to a pixel value for a given pixel format
   function MapRGBA (
      format : PixelFormat_ptr;
      r      : Uint8;
      g      : Uint8;
      b      : Uint8;
      a      : Uint8)
      return Uint32;
   pragma Import (C, MapRGBA, "SDL_MapRGBA");


   --  Maps a pixel value into the RGB components for a given pixel format
   procedure GetRGB (
      pixel : Uint32;
      fmt   : PixelFormat_ptr;
      r     : Uint8_ptr;
      g     : Uint8_ptr;
      b     : Uint8_ptr);
   pragma Import (C, GetRGB, "SDL_GetRGB");


   --  Maps a pixel value into the RGBA components for a given pixel format
   procedure GetRGBA (
      pixel : Uint32;
      fmt   : PixelFormat_ptr;
      r     : Uint8_ptr;
      g     : Uint8_ptr;
      b     : Uint8_ptr;
      a     : Uint8_ptr);
   pragma Import (C, GetRGBA, "SDL_GetRGBA");

   --  Allocate and free an RGB surface (must be called after SetVideoMode)
   --  If the depth is 4 or 8 bits, an empty palette is allocated for the
   --  surface. If the depth is greater than 8 bits, the pixel format is set
   --  using the flags '[RGB]mask'.
   --  If the function runs out of memory, it will return NULL.
   --
   --  The 'flags' tell what kind of surface to create.
   --  SWSURFACE means that the surface should be created in system memory.
   --  HWSURFACE means that the surface should be created in video memory,
   --  with the same format as the display surface.  This is useful for
   --  surfaces that will not change much, to take advantage of hardware
   --  acceleration when being blitted to the display surface.
   --  ASYNCBLIT means that SDL will try to perform asynchronous blits with
   --  this surface, but you must always lock it before accessing the pixels.
   --  SDL will wait for current blits to finish before returning from the
   --  lock. SRCCOLORKEY indicates that the surface will be used for colorkey
   --  blits. If the hardware supports acceleration of colorkey blits between
   --  two surfaces in video memory, SDL will try to place the surface in
   --  video memory. If this isn't possible or if there is no hardware
   --  acceleration available, the surface will be placed in system memory.
   --  SRCALPHA means that the surface will be used for alpha blits and
   --  if the hardware supports hardware acceleration of alpha blits between
   --  two surfaces in video memory, to place the surface in video memory
   --  if possible, otherwise it will be placed in system memory.
   --  If the surface is created in video memory, blits will be _much_ faster,
   --  but the surface format must be identical to the video surface format,
   --  and the only way to access the pixels member of the surface is to use
   --  the LockSurface and UnlockSurface calls.
   --  If the requested surface actually resides in video memory, HWSURFACE
   --  will be set in the flags member of the returned surface.  If for some
   --  reason the surface could not be placed in video memory, it will not have
   --  the HWSURFACE flag set, and will be created in system memory instead.
   function CreateRGBSurface (
      flags  : Surface_Flags;
      width  : C.int;
      height : C.int;
      depth  : C.int;
      Rmask  : Uint32;
      Gmask  : Uint32;
      Bmask  : Uint32;
      Amask  : Uint32)
      return Surface_ptr;
   pragma Import (C, CreateRGBSurface, "SDL_CreateRGBSurface");

   function CreateRGBSurfaceFrom (
      pixels : void_ptr;
      width  : C.int;
      height : C.int;
      depth  : C.int;
      pitch  : C.int;
      Rmask  : Uint32;
      Gmask  : Uint32;
      Bmask  : Uint32;
      Amask  : Uint32)
      return Surface_ptr;
   pragma Import (C, CreateRGBSurfaceFrom, "SDL_CreateRGBSurfaceFrom");

   procedure FreeSurface (surface : Surface_ptr);
   pragma Import (C, FreeSurface, "SDL_FreeSurface");

   function AllocSurface (
      flags  : Surface_Flags;
      width  : C.int;
      height : C.int;
      depth  : C.int;
      Rmask  : Uint32;
      Gmask  : Uint32;
      Bmask  : Uint32;
      Amask  : Uint32)
      return Surface_ptr
   renames CreateRGBSurface;

   --  LockSurface sets up a surface for directly accessing the pixels.
   --  Between calls to LockSurface/UnlockSurface, you can write
   --  to and read from 'the_surface_ptr.pixels', using the pixel format
   --  stored in  'the_surface_ptr.format'.  Once you are done accessing
   --  the surface, you should use UnlockSurface to release it.
   --
   --  Not all surfaces require locking.  If MUSTLOCK(surface) evaluates
   --  to 0, then you can read and write to the surface at any time, and the
   --  pixel format of the surface will not change.  In particular, if the
   --  HWSURFACE flag is not given when calling SetVideoMode, you
   --  will not need to lock the display surface before accessing it.
   --
   --  No operating system or library calls should be made between lock/unlock
   --  pairs, as critical system locks may be held during this time.
   --
   --  LockSurface returns 0, or -1 if the surface couldn't be locked.
   function LockSurface (surface : Surface_ptr) return C.int;
   pragma Import (C, LockSurface, "SDL_LockSurface");

   --  Removes any hardware locks, enabling blits
   procedure UnlockSurface (surface : Surface_ptr);
   pragma Import (C, UnlockSurface, "SDL_UnlockSurface");

   --  Load a surface from a seekable SDL data source (memory or file.)
   --  If 'freesrc' is non-zero, the source will be closed after being read.
   --  Returns the new surface, or NULL if there was an error.
   --  The new surface should be freed with FreeSurface.
   function LoadBMP_RW (
      src     : SDL.RWops.RWops_ptr;
      freesrc : C.int)
      return Surface_ptr;
   pragma Import (C, LoadBMP_RW, "SDL_LoadBMP_RW");

   --  Load a surface from a file.
   function LoadBMP (file : C.Strings.chars_ptr) return Surface_ptr;
   pragma Inline (LoadBMP);

   function LoadBMP (file : String) return Surface_ptr;
   pragma Inline (LoadBMP);

   function Load_BMP (file : String) return Surface_ptr
      renames LoadBMP;

   --  Save a surface to a seekable SDL data source (memory or file.)
   --  If 'freedst' is non-zero, the source will be closed after being written.
   --  Returns 0 if successful or -1 if there was an error.
   function SaveBMP_RW (
      surface : Surface_ptr;
      dst     : SDL.RWops.RWops_ptr;
      freedst : C.int)
      return C.int;
   pragma Import (C, SaveBMP_RW, "SDL_SaveBMP_RW");

   --  Save a surface to a file
   function SaveBMP (
      surface : Surface_ptr;
      file    : C.Strings.chars_ptr)
      return C.int;
   pragma Inline (SaveBMP);

   procedure SaveBMP (
      surface : Surface_ptr;
      file    : C.Strings.chars_ptr);
   pragma Inline (SaveBMP);

   function Save_BMP (
      surface : Surface_ptr;
      file    : String)
      return C.int;
   pragma Inline (Save_BMP);


   procedure Save_BMP (
      surface : Surface_ptr;
      file    : String);
   pragma Inline (Save_BMP);


   --  Sets the color key (transparent pixel) in a blittable surface.
   --  If 'flag' is SRCCOLORKEY (optionally OR'd with RLEACCEL),
   --  'key' will be the transparent pixel in the source image of a blit.
   --  RLEACCEL requests RLE acceleration for the surface if present,
   --  and removes RLE acceleration if absent.
   --  If 'flag' is 0, this function clears any current color key.
   --  This function returns 0, or -1 if there was an error.
   function SetColorKey (
      surface : Surface_ptr;
      --  flag    : Uint32;
      flag : Surface_Flags;
      key     : Uint32)
      return C.int;

   procedure SetColorKey (
      surface : Surface_ptr;
      --  flag    : Uint32;
      flag : Surface_Flags;
      key     : Uint32);

   pragma Import (C, SetColorKey, "SDL_SetColorKey");

   --  This function sets the alpha value for the entire surface, as opposed to
   --  using the alpha component of each pixel. This value measures the range
   --  of transparency of the surface, 0 being completely transparent to 255
   --  being completely opaque. An 'alpha' value of 255 causes blits to be
   --  opaque, the source pixels copied to the destination (the default). Note
   --  that per-surface alpha can be combined with colorkey transparency.
   --
   --  If 'flag' is 0, alpha blending is disabled for the surface.
   --  If 'flag' is SRCALPHA, alpha blending is enabled for the surface.
   --  OR:ing the flag with RLEACCEL requests RLE acceleration for the
   --  surface; if RLEACCEL is not specified, the RLE accel will be removed.
   function SetAlpha (
      surface : Surface_ptr;
      flag    : Surface_Flags;
      alpha   : Uint8)
      return C.int;

   procedure SetAlpha (
      surface : Surface_ptr;
      flag    : Surface_Flags;
      alpha   : Uint8);

   pragma Import (C, SetAlpha, "SDL_SetAlpha");

   --  Sets the clipping rectangle for the destination surface in a blit.
   --
   --  If the clip rectangle is NULL, clipping will be disabled.
   --  If the clip rectangle doesn't intersect the surface, the function will
   --  return SDL_FALSE and blits will be completely clipped.  Otherwise the
   --  function returns SDL_TRUE and blits to the surface will be clipped to
   --  the intersection of the surface area and the clipping rectangle.
   --
   --  Note that blits are automatically clipped to the edges of the source
   --  and destination surfaces.
   function SetClipRect (
      surface : Surface_ptr;
      rect    : Rect_ptr)
      return C.int;  -- The return must be SDL_true or SDL_false,

   function SetClipRect (
      surface : Surface_ptr;
      the_rect    : Rect)
      return C.int;  -- The return must be SDL_true or SDL_false,

   procedure SetClipRect (
      surface : Surface_ptr;
      rect    : Rect_ptr);

   procedure SetClipRect (
      surface : Surface_ptr;
      the_rect    : Rect);

   pragma Import (C, SetClipRect, "SDL_SetClipRect");

   procedure Disable_Clipping (surface : Surface_ptr);
   pragma Inline (Disable_Clipping);

   --  Gets the clipping rectangle for the destination surface in a blit.
   --  'prect' must be a pointer to a valid rectangle which will be filled
   --   with the correct values.
   procedure GetClipRect (
      surface : Surface_ptr;
      prect    : access Rect);

   procedure GetClipRect (
      surface  : Surface_ptr;
      the_rect : Rect);

   pragma Import (C, GetClipRect, "SDL_GetClipRect");


   --  Creates a new surface of the specified format, and then copies and maps
   --  the given surface to it so the blit of the converted surface will be as
   --  fast as possible.  If this function fails, it returns NULL.
   --
   --  The 'flags' parameter is passed to CreateRGBSurface and has those
   --  semantics.
   --
   --  This function is used internally by SDL_DisplayFormat.
   function ConvertSurface (
      src   : Surface_ptr;
      fmt   : PixelFormat_ptr;
      flags : Surface_Flags)
      return Surface_ptr;
   pragma Import (C, ConvertSurface, "SDL_ConvertSurface");

   --  This performs a fast blit from the source surface to the destination
   --  surface.  It assumes that the source and destination rectangles are
   --  the same size.  If either 'srcrect' or 'dstrect' are NULL, the entire
   --  surface (src or dst) is copied.  The final blit rectangles are saved
   --  in 'srcrect' and 'dstrect' after all clipping is performed.
   --  If the blit is successful, it returns 0, otherwise it returns -1.
   --
   --  The blit function should not be called on a locked surface.
   --
   --  The blit semantics for surfaces with and without alpha and colorkey
   --  are defined as follows:
   --
   --  RGBA->RGB:
   --      SDL_SRCALPHA set:
   --       alpha-blend (using alpha-channel).
   --       SDL_SRCCOLORKEY ignored.
   --      SDL_SRCALPHA not set:
   --       copy RGB.
   --       if SDL_SRCCOLORKEY set, only copy the pixels matching the
   --       RGB values of the source colour key, ignoring alpha in the
   --       comparison.
   --
   --  RGB->RGBA:
   --      SDL_SRCALPHA set:
   --       alpha-blend (using the source per-surface alpha value);
   --       set destination alpha to opaque.
   --      SDL_SRCALPHA not set:
   --       copy RGB, set destination alpha to opaque.
   --      both:
   --       if SDL_SRCCOLORKEY set, only copy the pixels matching the
   --       source colour key.
   --
   --  RGBA->RGBA:
   --      SDL_SRCALPHA set:
   --       alpha-blend (using the source alpha channel) the RGB values;
   --       leave destination alpha untouched. [Note: is this correct?]
   --       SDL_SRCCOLORKEY ignored.
   --      SDL_SRCALPHA not set:
   --       copy all of RGBA to the destination.
   --       if SDL_SRCCOLORKEY set, only copy the pixels matching the
   --       RGB values of the source colour key, ignoring alpha in the
   --       comparison.
   --
   --  RGB->RGB:
   --      SDL_SRCALPHA set:
   --       alpha-blend (using the source per-surface alpha value).
   --      SDL_SRCALPHA not set:
   --       copy RGB.
   --      both:
   --       if SDL_SRCCOLORKEY set, only copy the pixels matching the
   --       source colour key.

   --  If either of the surfaces were in video memory, and the blit returns -2,
   --  the video memory was lost, so it should be reloaded with artwork and
   --  re-blitted:
   --   while BlitSurface (image, imgrect, screen, dstrect) = -2  loop
   --      while SDL_LockSurface (image) < 0 loop
   --         Sleep(10);
   --      end loop;
   --      -- Write image pixels to image->pixels --
   --      UnlockSurface (image);
   --   end loop;
   --  This happens under DirectX 5.0 when the system switches away from your
   --  fullscreen application.  The lock will also fail until you have access
   --  to the video memory again.

   --  This is the public blit function, BlitSurface, and it performs
   --  rectangle validation and clipping before passing it to LowerBlit
   function UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect_ptr)
      return C.int;

   function UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect_ptr)
      return C.int;

   function UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect)
      return C.int;

   function UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect)
      return C.int;

   procedure UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect_ptr);

   procedure UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect_ptr);

   procedure UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect);

   procedure UpperBlit (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect);

   pragma Import (C, UpperBlit, "SDL_UpperBlit");


   --  You should call BlitSurface unless you know exactly how SDL
   --  blitting works internally and how to use the other blit functions.
   function BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect_ptr)
      return C.int
   renames UpperBlit;

   function BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect_ptr)
      return C.int
   renames UpperBlit;

   function BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect)
      return C.int
   renames UpperBlit;

   function BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect)
      return C.int
   renames UpperBlit;

   procedure BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect_ptr)
   renames UpperBlit;

   procedure BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect_ptr)
   renames UpperBlit;

   procedure BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect)
   renames UpperBlit;

   procedure BlitSurface (
      src     : Surface_ptr;
      srcrect : Rect;
      dst     : Surface_ptr;
      dstrect : Rect)
   renames UpperBlit;

   --  This is a semi-private blit function and it performs low-level surface
   --  blitting only.
   function LowerBlit (
      src     : Surface_ptr;
      srcrect : Rect_ptr;
      dst     : Surface_ptr;
      dstrect : Rect_ptr)
      return C.int;
   pragma Import (C, LowerBlit, "SDL_LowerBlit");

   --  This function performs a fast fill of the given rectangle with 'color'
   --  The given rectangle is clipped to the destination surface clip area
   --  and the final fill rectangle is saved in the passed in pointer.
   --  If 'dstrect' is NULL, the whole surface will be filled with 'color'
   --  The color should be a pixel of the format used by the surface, and
   --  can be generated by the MapRGB function.
   --  This function returns 0 on success, or -1 on error.
   function FillRect (
      dst     : Surface_ptr;
      dstrect : Rect_ptr;
      color   : Uint32)
      return C.int;

   procedure FillRect (
      dst     : Surface_ptr;
      dstrect : Rect_ptr;
      color   : Uint32);

   procedure FillRect (
      dst     : Surface_ptr;
      dstrect : in out Rect; --  Not really changed inside FillRect.
      color   : Uint32);     --     but used to avoid some Unchecked_Access

   pragma Import (C, FillRect, "SDL_FillRect");

   --  This function takes a surface and copies it to a new surface of the
   --  pixel format and colors of the video framebuffer, suitable for fast
   --  blitting onto the display surface.  It calls ConvertSurface
   --
   --  If you want to take advantage of hardware colorkey or alpha blit
   --  acceleration, you should set the colorkey and alpha value before
   --  calling this function.
   --
   --  If the conversion fails or runs out of memory, it returns NULL
   function DisplayFormat (surface : Surface_ptr) return Surface_ptr;
   pragma Import (C, DisplayFormat, "SDL_DisplayFormat");

   --  This function takes a surface and copies it to a new surface of the
   --  pixel format and colors of the video framebuffer (if possible),
   --  suitable for fast alpha blitting onto the display surface.
   --  The new surface will always have an alpha channel.
   --
   --  If you want to take advantage of hardware colorkey or alpha blit
   --  acceleration, you should set the colorkey and alpha value before
   --  calling this function.
   --
   --  If the conversion fails or runs out of memory, it returns NULL
   function DisplayFormatAlpha (surface : Surface_ptr) return Surface_ptr;
   pragma Import (C, DisplayFormatAlpha, "SDL_DisplayFormatAlpha");

   --  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   --  * YUV video surface overlay functions                               *
   --  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   --  This function creates a video output overlay
   --  Calling the returned surface an overlay is something of a misnomer
   --  because the contents of the display surface underneath the area where
   --  the overlay is shown is undefined - it may be overwritten with the
   --  converted YUV data.
   function CreateYUVOverlay (
      width   : C.int;
      height  : C.int;
      format  : Uint32;
      display : Surface_ptr)
      return Overlay_ptr;
   pragma Import (C, CreateYUVOverlay, "SDL_CreateYUVOverlay");


   --  Lock an overlay for direct access, and unlock it when you are done
   function LockYUVOverlay (overlay : Overlay_ptr) return C.int;
   pragma Import (C, LockYUVOverlay, "SDL_LockYUVOverlay");

   procedure UnlockYUVOverlay (overlay : Overlay_ptr);
   pragma Import (C, UnlockYUVOverlay, "SDL_UnlockYUVOverlay");

   --  Blit a video overlay to the display surface.
   --  The contents of the video surface underneath the blit destination are
   --  not defined.
   --  The width and height of the destination rectangle may be different from
   --  that of the overlay, but currently only 2x scaling is supported.
   function DisplayYUVOverlay (
      overlay : Overlay_ptr;
      dstrect : Rect_ptr)
      return C.int;
   pragma Import (C, DisplayYUVOverlay, "SDL_DisplayYUVOverlay");

   --  Free a video overlay
   procedure FreeYUVOverlay (overlay : Overlay_ptr);
   pragma Import (C, FreeYUVOverlay, "SDL_FreeYUVOverlay");

   --  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   --  * OpenGL support functions.                                     *
   --  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   --  *
   --  * Dynamically load a GL driver, if SDL is built with dynamic GL.
   --  *
   --  * SDL links normally with the OpenGL library on your system by default,
   --  * but you can compile it to dynamically load the GL driver at runtime.
   --  * If you do this, you need to retrieve all of the GL functions used in
   --  * your program from the dynamic library using GL_GetProcAddress.
   --  *
   --  * This is disabled in default builds of SDL.
   function GL_LoadLibrary (path : C.Strings.chars_ptr) return C.int;
   pragma Import (C, GL_LoadLibrary, "SDL_GL_LoadLibrary");


   --  Get the address of a GL function (for extension functions)
   procedure GL_GetProcAddress (proc : C.Strings.chars_ptr);
   pragma Import (C, GL_GetProcAddress, "SDL_GL_GetProcAddress");


   --  Set an attribute of the OpenGL subsystem before intialization.
   function GL_SetAttribute (
      attr  : GLattr;
      value : C.int)
      return C.int;

   procedure GL_SetAttribute (
      attr  : GLattr;
      value : C.int);

   pragma Import (C, GL_SetAttribute, "SDL_GL_SetAttribute");

   --  Get an attribute of the OpenGL subsystem from the windowing
   --  interface, such as glX. This is of course different from getting
   --  the values from SDL's internal OpenGL subsystem, which only
   --  stores the values you request before initialization.
   --
   --  Developers should track the values they pass into GL_SetAttribute
   --  themselves if they want to retrieve these values.
   function GL_GetAttribute (
      attr  : GLattr;
      value : access C.int)
      return C.int;

   procedure GL_GetAttribute (
      attr  : GLattr;
      value : access C.int);

   procedure  GL_GetAttribute (
      attr  : GLattr;
      value : out C.int);

   pragma Import (C, GL_GetAttribute, "SDL_GL_GetAttribute");


   --  Swap the OpenGL buffers, if double-buffering is supported.
   procedure GL_SwapBuffers;
   pragma Import (C, GL_SwapBuffers, "SDL_GL_SwapBuffers");

   --  ----------------------------------------------------
   --  Internal functions that should not be called unless you have read
   --  and understood the source code for these functions.

   procedure GL_UpdateRects (
      numrects : C.int;
      rects    : Rect_ptr);
   pragma Import (C, GL_UpdateRects, "SDL_UpdateRects");

   procedure GL_Lock;
   pragma Import (C, GL_Lock, "SDL_GL_Lock");

   procedure GL_Unlock;
   pragma Import (C, GL_Unlock, "SDL_GL_Unlock");


   --  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   --  * These functions allow interaction with the window manager, if any.  *
   --  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   --  *
   --  * Sets/Gets the title and icon text of the display window
   procedure WM_SetCaption (
      title : C.Strings.chars_ptr;
      icon  : C.Strings.chars_ptr);
   pragma Import (C, WM_SetCaption, "SDL_WM_SetCaption");

   procedure WM_Set_Caption (
      title : in String;
      icon  : in String);
   pragma Inline (WM_Set_Caption);

   procedure WM_Set_Caption_Title (title : in String);
   pragma Inline (WM_Set_Caption_Title);

   procedure WM_Set_Caption_Icon (icon  : in String);
   pragma Inline (WM_Set_Caption_Icon);

   procedure WM_GetCaption (
      title : chars_ptr_ptr;
      icon  : chars_ptr_ptr);
   pragma Import (C, WM_GetCaption, "SDL_WM_GetCaption");

   procedure WM_Get_Caption (
      title : out US.Unbounded_String;
      icon  : out US.Unbounded_String);
   pragma Inline (WM_Get_Caption);

   procedure WM_Get_Caption_Title (title : out US.Unbounded_String);
   pragma Inline (WM_Get_Caption_Title);

   procedure WM_Get_Caption_Icon (icon : out US.Unbounded_String);
   pragma Inline (WM_Get_Caption_Icon);

   --  Sets the icon for the display window.
   --  This function must be called before the first call to SetVideoMode.
   --  It takes an icon surface, and a mask in MSB format.
   --  If 'mask' is NULL, the entire icon surface will be used as the icon.
   procedure WM_SetIcon (
      icon : Surface_ptr;
      mask : Uint8_ptr); --  Allowing "null"

   type Icon_Mask_Array is
      array (Integer range <>) of Uint8;
   pragma Convention (C, Icon_Mask_Array);

   procedure WM_SetIcon (
      icon : Surface_ptr;
      mask : in Icon_Mask_Array);
   pragma Import (C, WM_SetIcon, "SDL_WM_SetIcon");

   --  This function iconifies the window, and returns 1 if it succeeded.
   --  If the function succeeds, it generates an APPACTIVE loss event.
   --  This function is a noop and returns 0 in non-windowed environments.
   function WM_IconifyWindow return C.int;
   procedure WM_IconifyWindow;
   pragma Import (C, WM_IconifyWindow, "SDL_WM_IconifyWindow");

   --  Toggle fullscreen mode without changing the contents of the screen.
   --  If the display surface does not require locking before accessing
   --  the pixel information, then the memory pointers will not change.
   --
   --  If this function was able to toggle fullscreen mode (change from
   --  running in a window to fullscreen, or vice-versa), it will return 1.
   --  If it is not implemented, or fails, it returns 0.
   --
   --  The next call to SetVideoMode will set the mode fullscreen
   --  attribute based on the flags parameter - if SDL_FULLSCREEN is not
   --  set, then the display will be windowed by default where supported.
   --
   --  This is currently only implemented in the X11 video driver.
   function WM_ToggleFullScreen (surface : Surface_ptr) return C.int;
   pragma Import (C, WM_ToggleFullScreen, "SDL_WM_ToggleFullScreen");

   type GrabMode is new C.int;
   GRAB_QUERY      : constant GrabMode := -1;
   GRAB_OFF        : constant GrabMode :=  0;
   GRAB_ON         : constant GrabMode :=  1;
   GRAB_FULLSCREEN : constant GrabMode :=  2; -- Used internally

--  This function allows you to set and query the input grab state of
   --  the application.  It returns the new input grab state.

   --  Grabbing means that the mouse is confined to the application window,
   --  and nearly all keyboard input is passed directly to the application,
   --  and not interpreted by a window manager, if any.
   --  function WM_GrabInput (
   --     mode : C.int    -- Might be GRAB_QUERY, GRAB_OFF, or GRAB_FULLSCREEN
   --     ) return C.int; -- Might be GRAB_QUERY, GRAB_OFF, or GRAB_FULLSCREEN
   function WM_GrabInput (mode : GrabMode) return GrabMode;
   pragma Import (C, WM_GrabInput, "SDL_WM_GrabInput");

end SDL.Video;
