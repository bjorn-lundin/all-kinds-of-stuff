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
with SDL.Error;

package body SDL.Video.Textures is
   package C renames Interfaces.C;

   use type C.int;
   use type SDL.C_Pointers.Texture_Pointer;

   procedure Destroy (Self : in out Texture) is
      procedure SDL_Destroy_Texture (T : in SDL.C_Pointers.Texture_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyTexture";
   begin
      SDL_Destroy_Texture (Self.Internal);

      Self.Internal := null;
   end Destroy;

   function Get_Alpha (Self : in Texture) return SDL.Video.Palettes.Colour_Component is
      function SDL_Get_Texture_Alpha_Mod (T     : in SDL.C_Pointers.Texture_Pointer;
                                          Alpha : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureAlphaMod";

      Data   : SDL.Video.Palettes.Colour_Component;
      Result : C.int := SDL_Get_Texture_Alpha_Mod (Self.Internal, Data);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Alpha;

   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL.Video.Palettes.Colour_Component) is
      function SDL_Set_Texture_Alpha_Mod (T     : in SDL.C_Pointers.Texture_Pointer;
                                          Alpha : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureAlphaMod";

      Result : C.int := SDL_Set_Texture_Alpha_Mod (Self.Internal, Alpha);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Alpha;

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes is
      function SDL_Get_Texture_Blend_Mode (T     : in SDL.C_Pointers.Texture_Pointer;
                                           Blend : out Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureBlendMode";

      Data   : Blend_Modes;
      Result : C.int := SDL_Get_Texture_Blend_Mode (Self.Internal, Data);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes) is
      function SDL_Set_Texture_Blend_Mode (T    : in SDL.C_Pointers.Texture_Pointer;
                                           Mode : in Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureBlendMode";

      Result : C.int := SDL_Set_Texture_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Modulate_Colour (Self : in Texture) return SDL.Video.Palettes.RGB_Colour is
      function SDL_Get_Texture_Color_Mod (T       : in SDL.C_Pointers.Texture_Pointer;
                                          R, G, B : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureColorMod";

      Data   : SDL.Video.Palettes.RGB_Colour;
      Result : C.int := SDL_Get_Texture_Color_Mod (Self.Internal, Data.Red, Data.Green, Data.Blue);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Modulate_Colour;

   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL.Video.Palettes.RGB_Colour) is
      function SDL_Set_Texture_Color_Mod (T       : in SDL.C_Pointers.Texture_Pointer;
                                          R, G, B : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureColorMod";

      Result : C.int := SDL_Set_Texture_Color_Mod (Self.Internal, Colour.Red, Colour.Green, Colour.Blue);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Modulate_Colour;
   
   
   --bnl
   procedure Query(Self   : in out Texture; 
                   Format : in out Natural;
                   Acess  : in out Interfaces.C.Int;
                   W      : in out Interfaces.C.Int;
                   H      : in out Interfaces.C.Int) is
                   
      function SDL_Query_Texture (T       : SDL.C_Pointers.Texture_Pointer;
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
         raise Texture_Error with SDL.Error.Get;
      end if;
      
      Format := Natural(Local_Format);
      Acess  := Local_Acess;
      W      := Local_W;
      H      := Local_H;
      
   end Query;   
   
   --bnl
   
   

   --     procedure Lock_Texture (Self   : in out Texture;
   --                             Pixels : out SDL.Video.Pixels.Pixel_ARGB_8888_Array_Access) is
   --        type Int_Ptr is access C.int with
   --          Convention => C;
   --
   --        function SDL_Lock_Texture (T      : in System.Address;
   --                                   Area   : in System.Address;
   --                                   Pixels : out SDL.Video.Pixels.C_Pixel_Ptr;
   --                                   Pitch  : out Int_Ptr) return C.int with
   --          Import        => True,
   --          Convention    => C,
   --          External_Name => "SDL_LockTexture";
   --
   --        C_Pixels : SDL.Video.Pixels.C_Pixel_Ptr := null;
   --        C_Pitch  : Int_Ptr        := null;
   --        Result   : C.int          := SDL_Lock_Texture (Self.Internal, System.Null_Address, C_Pixels, C_Pitch);
   --     begin
   --        if Result /= Success then
   --           raise Texture_Error with SDL.Error.Get;
   --        end if;
   --
   --        Self.Locked := True;
   --
   --        Pixels := SDL.Video.Pixels.Create (C_Pixels, C_Pitch.all, Self.Size);
   --     end Lock_Texture;

   procedure Lock (Self    : in out Texture;
                   Pixels  : out SDL.Video.Pixels.ARGB_8888_Access.Pointer;
                   Pitches : out SDL.Video.Pixels.Pitch_Access.Pointer) is
      function SDL_Lock_Texture (T       : in SDL.C_Pointers.Texture_Pointer;
                                 Area    : in System.Address; --  TODO??
                                 Pixels  : out SDL.Video.Pixels.ARGB_8888_Access.Pointer;
                                 Pitches : out SDL.Video.Pixels.Pitch_Access.Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LockTexture";

      Result : C.int := SDL_Lock_Texture (Self.Internal, System.Null_Address, Pixels, Pitches);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      Self.Locked := True;
   end Lock;

   procedure Unlock (Self : in out Texture) is
      procedure SDL_Unlock_Texture (T : in SDL.C_Pointers.Texture_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UnlockTexture";
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

   function Get_Internal_Texture (Self : in Texture) return SDL.C_Pointers.Texture_Pointer is
   begin
      return Self.Internal;
   end Get_Internal_Texture;
end SDL.Video.Textures;
