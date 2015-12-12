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
with Interfaces.C;
with Ada.Unchecked_Conversion;
with SDL2.Error;

package body SDL2.Video.Textures is
   package C renames Interfaces.C;

   use type C.int;

   procedure Destroy (Self : in out Texture) is
      procedure SDL_Destroy_Texture (T : in Texture_Pointer) ;
      pragma Import (C, SDL_Destroy_Texture, "SDL_DestroyTexture");
   begin
      SDL_Destroy_Texture (Self.Internal);
      Self.Internal := null;
   end Destroy;

   function Get_Alpha (Self : in Texture) return SDL2.Video.Palettes.Colour_Component is
      function SDL_Get_Texture_Alpha_Mod (T     : in Texture_Pointer;
                                          Alpha : out SDL2.Video.Palettes.Colour_Component) return C.int;
      pragma Import (C, SDL_Get_Texture_Alpha_Mod, "SDL_GetTextureAlphaMod");
      Data   : SDL2.Video.Palettes.Colour_Component;
      Result : C.int := SDL_Get_Texture_Alpha_Mod (Self.Internal, Data);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      return Data;
   end Get_Alpha;

   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL2.Video.Palettes.Colour_Component) is
      function SDL_Set_Texture_Alpha_Mod (T     : in Texture_Pointer;
                                          Alpha : in SDL2.Video.Palettes.Colour_Component) return C.int ;
      pragma Import (C, SDL_Set_Texture_Alpha_Mod, "SDL_SetTextureAlphaMod");
      Result : C.int := SDL_Set_Texture_Alpha_Mod (Self.Internal, Alpha);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
   end Set_Alpha;

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes is
      type Blend_Modes_Ptr is access all Blend_Modes;
      function SDL_Get_Texture_Blend_Mode (T     : in  Texture_Pointer;
                                           Blend : out Blend_Modes_Ptr) return C.int ;
      pragma Import (C, SDL_Get_Texture_Blend_Mode, "SDL_GetTextureBlendMode");
      Data   : aliased Blend_Modes;
      Result : C.int := SDL_Get_Texture_Blend_Mode (Self.Internal, Data'access);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      return Data;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes) is
      function SDL_Set_Texture_Blend_Mode (T    : in Texture_Pointer;
                                           Mode : in Blend_Modes) return C.int ;
      pragma Import (C, SDL_Set_Texture_Blend_Mode, "SDL_SetTextureBlendMode");
      Result : C.int := SDL_Set_Texture_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Modulate_Colour (Self : in Texture) return SDL2.Video.Palettes.RGB_Colour is
      type Colour_Component_Ptr is access all SDL2.Video.Palettes.Colour_Component;
      function SDL_Get_Texture_Color_Mod (T       : in Texture_Pointer;
                                          R, G, B : out Colour_Component_Ptr) return C.int ;
      pragma Import (C, SDL_Get_Texture_Color_Mod, "SDL_GetTextureColorMod");
      R, G, B : aliased SDL2.Video.Palettes.Colour_Component;
      Result : C.int := SDL_Get_Texture_Color_Mod (Self.Internal,R'access, G'access, B'access);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      return SDL2.Video.Palettes.RGB_Colour'(Red => R, Green => G, Blue => B);
   end Get_Modulate_Colour;

   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL2.Video.Palettes.RGB_Colour) is
      function SDL_Set_Texture_Color_Mod (T       : in Texture_Pointer;
                                          R, G, B : in SDL2.Video.Palettes.Colour_Component) return C.int ;
      pragma Import (C, SDL_Set_Texture_Color_Mod, "SDL_SetTextureColorMod");
      Result : C.int := SDL_Set_Texture_Color_Mod (Self.Internal, Colour.Red, Colour.Green, Colour.Blue);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
   end Set_Modulate_Colour;
   
   
   --bnl
   procedure Query(Self   : in out Texture; 
                   Format : in out Natural;
                   Acess  : in out Interfaces.C.Int;
                   W      : in out Interfaces.C.Int;
                   H      : in out Interfaces.C.Int) is
                   
      function SDL_Query_Texture (T       : Texture_Pointer;
                                  F       : access Interfaces.C.Long ;
                                  A, W, H : access Interfaces.C.Int) return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_QueryTexture";

      Local_Format : aliased Interfaces.C.Long  :=  Interfaces.C.Long(Format);
      Local_Acess  : aliased Interfaces.C.Int := Acess;
      Local_W      : aliased Interfaces.C.Int := W;
      Local_H      : aliased Interfaces.C.Int := H;
        
      Result : C.Int := SDL_Query_Texture (Self.Internal, 
                                           Local_Format'access, 
                                           Local_Acess'access, 
                                           Local_W'access, 
                                           Local_H'access);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      
      Format := Natural(Local_Format);
      Acess  := Local_Acess;
      W      := Local_W;
      H      := Local_H;
      
   end Query;   
   
   --bnl
   
   

   --     procedure Lock_Texture (Self   : in out Texture;
   --                             Pixels : out SDL2.Video.Pixels.Pixel_ARGB_8888_Array_Access) is
   --        type Int_Ptr is access C.int with
   --          Convention => C;
   --
   --        function SDL_Lock_Texture (T      : in System.Address;
   --                                   Area   : in System.Address;
   --                                   Pixels : out SDL2.Video.Pixels.C_Pixel_Ptr;
   --                                   Pitch  : out Int_Ptr) return C.int with
   --          Import        => True,
   --          Convention    => C,
   --          External_Name => "SDL_LockTexture";
   --
   --        C_Pixels : SDL2.Video.Pixels.C_Pixel_Ptr := null;
   --        C_Pitch  : Int_Ptr        := null;
   --        Result   : C.int          := SDL_Lock_Texture (Self.Internal, System.Null_Address, C_Pixels, C_Pitch);
   --     begin
   --        if Result /= Success then
   --           raise Texture_Error with SDL2.Error.Get;
   --        end if;
   --
   --        Self.Locked := True;
   --
   --        Pixels := SDL2.Video.Pixels.Create (C_Pixels, C_Pitch.all, Self.Size);
   --     end Lock_Texture;

   procedure Lock (Self    : in out Texture;
                   Pixels  : out SDL2.Video.Pixels.ARGB_8888_Access.Pointer;
                   Pitches : out SDL2.Video.Pixels.Pitch_Access.Pointer) is
      function SDL_Lock_Texture (T       : in Texture_Pointer;
                                 Area    : in System.Address; --  TODO??
                                 Pixels  : out SDL2.Video.Pixels.ARGB_8888_Access.Pointer;
                                 Pitches : out SDL2.Video.Pixels.Pitch_Access.Pointer) return C.int ;
      pragma Import (C, SDL_Lock_Texture, "SDL_LockTexture");
      Result : C.int := SDL_Lock_Texture (Self.Internal, System.Null_Address, Pixels, Pitches);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;

      Self.Locked := True;
   end Lock;

   procedure Unlock (Self : in out Texture) is
      procedure SDL_Unlock_Texture (T : in Texture_Pointer) ;
      pragma Import (C, SDL_Unlock_Texture, "SDL_UnlockTexture");
   begin
      if Self.Locked then
         SDL_Unlock_Texture (Self.Internal);
         Self.Locked := False;
      end if;
   end Unlock;

   overriding
   procedure Finalize (Self : in out Texture) is
   begin
      if Self.Internal /= null and then Self.Owns then
         Destroy (Self);
      end if;
   end Finalize;

   function Get_Internal (Self : in Texture) return Texture_Pointer is
   begin
      return Self.Internal;
   end Get_Internal;
   
   
    procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Format   : in SDL.Video.Pixel_Formats.Pixel_Format_Names;
      Kind     : in Kinds;
      Size     : in SDL.Video.Windows.Sizes) is

      --  Convert the Pixel_Format_Name to an Unsigned_32 because the compiler is changing the value somewhere along
      --  the lines from the start of this procedure to calling SDL_Create_Texture.
      function To_Unsigned32 is new Ada.Unchecked_Conversion (Source => SDL.Video.Pixel_Formats.Pixel_Format_Names,
                                                              Target => Interfaces.Unsigned_32);

      function SDL_Create_Texture
        (R      : in Renderer_Pointer;
         Format : in Interfaces.Unsigned_32;
         Kind   : in Kinds;
         W, H   : in C.int) return Texture_Pointer ;
         pragma Import (C, SDL_Create_Texture, "SDL_CreateTexture");

   begin
      Tex.Internal := SDL_Create_Texture (Renderer.Get_Internal ,
                                          To_Unsigned32 (Format),
                                          Kind,
                                          C.int (Size.Width),
                                          C.int (Size.Height));
      if Tex.Internal = null then
         raise Texture_Error with SDL.Error.Get;
      end if;
      Tex.Size         := Size;
      Tex.Pixel_Format := Format;
   end Create;

   procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Surface  : in SDL.Video.Surfaces.Surface) is

      function SDL_Create_Texture_Form_Surface (R : in  SDL2.Video.Renderers.Renderer_Pointer;
                                                S : in  SDL2.Video.Surfaces.Surface_Pointer)
                                                return Texture_Pointer ;
      pragma Import (C, SDL_Create_Texture_Form_Surface, "SDL_CreateTextureFromSurface");
   begin
      Tex.Internal := SDL_Create_Texture_Form_Surface (Renderer.Get_Internal,
                                                       Surface.Get_Interal);

      if Tex.Internal = null then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Create;
  
   
end SDL2.Video.Textures;
