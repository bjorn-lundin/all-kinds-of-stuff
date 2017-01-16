unit game;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,Crt, SDL2, SDL2_ttf,SDL2_image;


procedure Start;
procedure Test1;


implementation


type TGame=Class
  sdlWindow   : PSDL_Window;
  sdlRenderer : PSDL_Renderer;
  sdlTexture  : PSDL_Texture;
public
  procedure Initiate;
  procedure Drawfirstrow;
  procedure Close;

end;


procedure TGame.Close;
begin
  Writeln('Class destructor TGame');
  SDL_DestroyTexture(Self.sdlTexture );
  SDL_DestroyRenderer(Self.sdlRenderer );
  SDL_DestroyWindow (Self.sdlWindow );
  SDL_Quit;
end;


var Global_Game : TGame;

procedure TGame.Initiate;
begin
  //initilization of video subsystem
  if SDL_Init( SDL_INIT_VIDEO ) < 0 then HALT;

  Self.sdlWindow := SDL_CreateWindow( 'Pac man', 50, 50, 3*225, 3*245, SDL_WINDOW_SHOWN );
  if Self.sdlWindow = nil then HALT;

  Self.sdlRenderer := SDL_CreateRenderer( Self.sdlWindow, -1, 0 );
  if Self.sdlRenderer = nil then HALT;

  Self.sdlTexture := IMG_LoadTexture( Self.sdlRenderer, 'assets\pacman_sprite.png' );
  if Self.sdlTexture = nil then HALT;

  SDL_RenderCopy(Self.sdlRenderer, Self.sdlTexture, nil, nil );
  SDL_RenderPresent (Self.sdlRenderer);
  SDL_Delay( 2000 );

end;

procedure TGame.Drawfirstrow;
var
  x,y : integer;
  sdlRect1 : PSDL_Rect;
  sdlRect2 : PSDL_Rect;
begin
  SDL_RenderClear(Self.sdlRenderer);
  for x := 1 to 16 do begin
    for y := 1 to 4 do begin
      new(sdlRect1);
      sdlRect1^.x := ((x-1)*17); sdlRect1^.y := ((y-1)*17); sdlRect1^.w := 17; sdlRect1^.h := 17;
      new(sdlRect2);
      sdlRect2^.x := 0; sdlRect2^.y := 1; sdlRect2^.w := 3*17; sdlRect2^.h := 3*17;
      SDL_RenderCopy(Self.sdlRenderer,Self.sdlTexture, sdlRect1, sdlRect2);
      SDL_RenderPresent (Self.sdlRenderer);
      SDL_Delay(1000);
      dispose(sdlRect1);
      dispose(sdlRect2);
    end;
  end;
end;

procedure Start ;
begin
  Writeln('Start');
end;


procedure Test1 ;

begin
  Writeln('Test1 Start');
  Global_Game := TGame.Create;
  Global_Game.Initiate;
  Global_Game.Drawfirstrow;
  Global_Game.Close;
  Writeln('Test1 Stop');
end;

end.

