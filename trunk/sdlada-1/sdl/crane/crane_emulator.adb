
with Utils; use Utils;
with Obj; use Obj;
with SDL.Timer;
with SDL.Types; use SDL.Types;

procedure Crane_Emulator is
  FPS : constant Integer := 60; 
--  FPS : constant Integer := 1; 
  DELAY_TIME : constant Float := 1000.0 / Float(FPS);
  Game : Game_Type;
  Frame_Start, Frame_Time : Uint32;
  
  Player_0 : aliased SDL_Game_Object;
  Player_1 : aliased SDL_Game_Object;
  
  Players : Player_Array ;
  
begin
  Log("Crane_Emulator","start");
  
  
  Players(0).Game_Obj_Ptr := Player_0'unchecked_access;
  Players(1).Game_Obj_Ptr := Player_1'unchecked_access;
   
  
  Game.Init(Players);
  while Game.Running loop
    Frame_Start := SDL.Timer.GetTicks;
  
    Game.Handle_Events;
    Game.Update;
    Game.Render;
    Frame_Time := SDL.Timer.GetTicks - Frame_Start;
    if Float(Frame_Time) < DELAY_TIME then
      SDL.Timer.SDL_Delay(Uint32(DELAY_TIME) - Frame_Time);
    end if;
  end loop;  
  Log("Crane_Emulator","stop");
end Crane_Emulator;
  