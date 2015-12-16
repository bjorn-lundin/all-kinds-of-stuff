--------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with Ada.Unchecked_Conversion;
with SDL2.Error;

package body SDL2.Video.Textures is
   package C renames Interfaces.C;

   use type C.Int;
   use type SDL2.Texture_Pointer;

   procedure Destroy (Self : in out Texture) is
      procedure SDL_DestroyTexture (T : in SDL2.Texture_Pointer);
      pragma Import(C, SDL_DestroyTexture,  "SDL_DestroyTexture");
   begin
      SDL_Destroy_Texture (Self.Pointer);
      Self.Pointer := null;
   end Destroy;

   function Get_Alpha (Self : in Texture) return SDL2.Video.Palettes.Colour_Component is
      function SDL_Get_Texture_Alpha_Mod (T     : in SDL2.Texture_Pointer;
                                          Alpha : access SDL2.Video.Palettes.Colour_Component) return C.Int ;
      pragma Import(C, SDL_Get_Texture_Alpha_Mod,  "SDL_GetTextureAlphaMod");
      Data   : aliased SDL2.Video.Palettes.Colour_Component;
      Result : C.Int := SDL_Get_Texture_Alpha_Mod (Self.Pointer, Data'access);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      return Data;
   end Get_Alpha;

   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL2.Video.Palettes.Colour_Component) is
      function SDL_Set_Texture_Alpha_Mod (T     : in SDL2.Texture_Pointer;
                                          Alpha : in SDL2.Video.Palettes.Colour_Component) return C.Int ;
      pragma Import(C, SDL_Set_Texture_Alpha_Mod,  "SDL_SetTextureAlphaMod");
      Result : C.Int := SDL_Set_Texture_Alpha_Mod (Self.Pointer, Alpha);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
   end Set_Alpha;

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes is
      function SDL_Get_Texture_Blend_Mode (T     : in SDL2.Texture_Pointer;
                                           Blend : access Blend_Modes) return C.Int ;
      pragma Import(C, SDL_Get_Texture_Blend_Mode,  "SDL_GetTextureBlendMode");

      Data   : aliased Blend_Modes;
      Result : C.Int := SDL_Get_Texture_Blend_Mode (Self.Pointer, Data'access);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      return Data;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes) is
      function SDL_Set_Texture_Blend_Mode (T    : in SDL2.Texture_Pointer;
                                           Mode : in Blend_Modes) return C.Int ;
      pragma Import(C, SDL_Set_Texture_Blend_Mode,  "SDL_SetTextureBlendMode");
      Result : C.Int := SDL_Set_Texture_Blend_Mode (Self.Pointer, Mode);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Modulate_Colour (Self : in Texture) return SDL2.Video.Palettes.RGB_Colour is
      function SDL_Get_Texture_Color_Mod (T       : in SDL2.Texture_Pointer;
                                          R, G, B : access SDL2.Video.Palettes.Colour_Component) return C.Int ;
      pragma Import(C, SDL_Get_Texture_Color_Mod,  "SDL_GetTextureColorMod");
      Red,Green,Blue : aliased SDL2.Video.Palettes.Colour_Component
      Data   : aliased SDL2.Video.Palettes.RGB_Colour;
      Result : C.Int := SDL_Get_Texture_Color_Mod (Self.Pointer, Red'access, Green'access, Blue'access);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      Data := (Red,Green,Blue);
      return Data;
   end Get_Modulate_Colour;

   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL2.Video.Palettes.RGB_Colour) is
      function SDL_Set_Texture_Color_Mod (T       : in SDL2.Texture_Pointer;
                                          R, G, B : in SDL2.Video.Palettes.Colour_Component) return C.Int ;
      pragma Import(C, SDL_Set_Texture_Color_Mod,  "SDL_SetTextureColorMod");

      Result : C.Int := SDL_Set_Texture_Color_Mod (Self.Pointer, Colour.Red, Colour.Green, Colour.Blue);
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
                   
      function SDL_Query_Texture (T       : SDL2.Texture_Pointer;
                                  F       : access Interfaces.C.Long ;
                                  A, W, H : access Interfaces.C.Int) return C.Int with
      pragma Import(C, SDL_Query_Texture,  "SDL_QueryTexture");

      Local_Format : aliased Interfaces.C.Long  :=  Interfaces.C.Long(Format);
      Local_Acess  : aliased Interfaces.C.Int := Acess;
      Local_W      : aliased Interfaces.C.Int := W;
      Local_H      : aliased Interfaces.C.Int := H;
        
      Result : C.Int := SDL_Query_Texture (Self.Pointer, 
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
   --        type Int_Ptr is access C.Int with
   --          Convention => C;
   --
   --        function SDL_Lock_Texture (T      : in System.Address;
   --                                   Area   : in System.Address;
   --                                   Pixels : out SDL2.Video.Pixels.C_Pixel_Ptr;
   --                                   Pitch  : out Int_Ptr) return C.Int with
   --          Import        => True,
   --          Convention    => C,
   --          External_Name => "SDL_LockTexture";
   --
   --        C_Pixels : SDL2.Video.Pixels.C_Pixel_Ptr := null;
   --        C_Pitch  : Int_Ptr        := null;
   --        Result   : C.Int          := SDL_Lock_Texture (Self.Pointer, System.Null_Address, C_Pixels, C_Pitch);
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
      function SDL_Lock_Texture (T       : in SDL2.Texture_Pointer;
                                 Area    : in System.Address; --  TODO??
                                 Pixels  : out SDL2.Video.Pixels.ARGB_8888_Access.Pointer;
                                 Pitches : out SDL2.Video.Pixels.Pitch_Access.Pointer) return C.Int ;
      pragma Import(C, SDL_Lock_Texture,  "SDL_LockTexture");
      Result : C.Int := SDL_Lock_Texture (Self.Pointer, System.Null_Address, Pixels, Pitches);
   begin
      if Result /= Success then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      Self.Locked := True;
   end Lock;

   procedure Unlock (Self : in out Texture) is
      procedure SDL_Unlock_Texture (T : in SDL2.Texture_Pointer) ;
      pragma Import(C, SDL_Unlock_Texture,  "SDL_UnlockTexture");
   begin
      if Self.Locked then
         SDL_Unlock_Texture (Self.Pointer);
         Self.Locked := False;
      end if;
   end Unlock;

   overriding
   procedure Finalize (Self : in out Texture) is
   begin
      if Self.Pointer /= null and then Self.Owner then
         Destroy (Self);
      end if;
   end Finalize;

   function Get_Internal (Self : in Texture) return SDL2.Texture_Pointer is
   begin
      return Self.Pointer;
   end Get_Internal;
end SDL2.Video.Textures;
