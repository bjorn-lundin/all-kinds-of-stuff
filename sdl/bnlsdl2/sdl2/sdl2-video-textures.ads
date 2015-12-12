--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  SDL2.Video.Textures
--
--  Texture abstraction.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with System;
with SDL2.Video.Palettes;
with SDL2.Video.Pixel_Formats;
with SDL2.Video.Pixels;
with SDL2.Video.Renderers;
with SDL2.Video.Surfaces;
with SDL2.Video.Windows;
with Interfaces.C; --bnl

package SDL2.Video.Textures is
   Texture_Error : exception;

   --  Was SDL_TextureAccess.
   type Kinds is (Static, Streaming, Target) with
     Convention => C;

   type Blend_Modes is (None, Alpha_Blend, Additive, Colour_Modulate) with
     Convention => C;

   for Blend_Modes use
     (None            => 16#0000_0000#,
      Alpha_Blend     => 16#0000_0001#,
      Additive        => 16#0000_0002#,
      Colour_Modulate => 16#0000_0004#);

      
   type Texture_Pointer;
      
   type Texture is new Ada.Finalization.Limited_Controlled with private;

   Null_Texture : constant Texture;

   
   procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL2.Video.Renderers.Renderer;
      Format   : in SDL2.Video.Pixel_Formats.Pixel_Format_Names;
      Kind     : in Kinds;
      Size     : in SDL2.Video.Windows.Sizes);

   procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL2.Video.Renderers.Renderer;
      Surface  : in SDL2.Video.Surfaces.Surface);
   
   procedure Destroy (Self : in out Texture);

   --  Get the alpha value to be multiplied (modulated) into render copy operations.
   function Get_Alpha (Self : in Texture) return SDL2.Video.Palettes.Colour_Component;
   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL2.Video.Palettes.Colour_Component);

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes;
   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes);

   function Get_Modulate_Colour (Self : in Texture) return SDL2.Video.Palettes.RGB_Colour;
   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL2.Video.Palettes.RGB_Colour);

   --  TODO: Fix this.
   --  Lock returns access to pixel data as write-only.
   --  function Lock (Self : in out Texture; Pixel_Data : out SDL2.Video.Pixels.Pixel) return Boolean with
   --  function Lock (Self : in out Texture; Area : in SDL2.Video.Rectangles.Rectangle;
   --    Pixel_Data : out SDL2.Video.Pixels.Pixel) return Boolean with
   --    Pre  => Self.Locked = False,
   --    Post => Result = True and then Self.Locked = True;
   --
   --  Lock should return an object representing the bitmap data. We should be able to access it like an array,
   --  e.g. (x, y) and also using an iterator, which traverses, top -> bottom, left -> right, 1 pixel at a time. We
   --  need to be able to do subimage copies using Ada's slices, e.g. bm1 (x .. x2, y .. y2) := bm2 (x .. x2, y .. y2)
   --
   --  For YV12 format:
   --

   --  package ARGB_8888_Array is new SDL2.Video.Pixels.Texture_Data (Width => , Height => , Element => );
   --     procedure Lock_Texture (Self   : in out Texture;
   --                             Pixels : out SDL2.Video.Pixels.Pixel_ARGB_8888_Array_Access);
   procedure Lock (Self    : in out Texture;
                   Pixels  : out SDL2.Video.Pixels.ARGB_8888_Access.Pointer;
                   Pitches : out SDL2.Video.Pixels.Pitch_Access.Pointer);
   procedure Unlock (Self : in out Texture);

   --  SDL_QueryTexture
   --bnl
   procedure Query(Self   : in out Texture; 
                   Format : in out Natural;
                   Acess  : in out Interfaces.C.Int;
                   W      : in out Interfaces.C.Int;
                   H      : in out Interfaces.C.Int);
                   
   function Get_Internal (Self : in Texture) return Texture_Pointer;
                   
   --bnl
   
   
   type Texture_Pointer is access all Texture with
     Convention => C;
   
   
   --  SDL_UpdateTexture
   --  SDL_UpdateYUVTexture
private
   type Texture is new Ada.Finalization.Limited_Controlled with
      record
         Internal     : Texture_Pointer             := null;
         Owns         : Boolean                                    := True;
         Locked       : Boolean                                    := False;
         Size         : SDL2.Video.Windows.Sizes                    := (Positive'First, Positive'First);
         Pixel_Format : SDL2.Video.Pixel_Formats.Pixel_Format_Names := SDL2.Video.Pixel_Formats.Pixel_Format_Unknown;
      end record;

   overriding
   procedure Finalize (Self : in out Texture);


   Null_Texture : constant Texture := (Ada.Finalization.Limited_Controlled with
                                       Internal     => null,
                                       Owns         => True,
                                       Size         => (Positive'First, Positive'First),
                                       Pixel_Format => Pixel_Formats.Pixel_Format_Unknown,
                                       Locked       => False);
end SDL2.Video.Textures;
