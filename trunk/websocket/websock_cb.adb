

------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Integer_Text_Io;
with Ada.Strings.Fixed;
with Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aws.Messages;
with Aws.Mime;
with Aws.Templates;
with Aws.Translator;
with Aws.Session;

with Gnat.Regpat;

with Notification_Center;

package body Websock_Cb is

  use Ada;
  use type Aws.Net.Websocket.Kind_Type;

  Www_Root : constant String := "../html";

  Cnt : Natural := 0;

  ------------
  -- Create --
  ------------
  function Create
    (Socket  : Net.Socket_Access;
     Request : Status.Data) return Net.Websocket.Object'Class is
  begin
    if Aws.Status.Has_Session(Request) then
      Text_Io.Put_Line ("Create - has session: True");
      Text_Io.Put_Line ("Create - session: " & AWS.Session.Image(Aws.Status.Session(Request)));
    else
      Text_Io.Put_Line ("Create - has session: False");
    end if;

    Cnt := Cnt +1;
    return Object'(Net.Websocket.Object
                   (Net.Websocket.Create (Socket, Request)) with C => Cnt);
  end Create;

  -----------
  -- HW_CB --
  -----------

  function Hw_Cb (Request : Status.Data) return Response.Data is
    Uri      : constant String := Status.Uri (Request);
    Filename : constant String := Uri (Uri'First + 1 .. Uri'Last);
  begin

    if Aws.Status.Has_Session(Request) then
      Text_Io.Put_Line ("Hw_Cb has session: True");
      Text_Io.Put_Line ("Hw_Cb session: " & AWS.Session.Image(Aws.Status.Session(Request)));
    else
      Text_Io.Put_Line ("Hw_Cb has session: False");
    end if;



    if Uri'Length = 12
      and then Uri (Uri'First .. Uri'First + 11) = "/favicon.ico"
    then
      return Aws.Response.Acknowledge (Messages.S404);
    else
      return Response.File("text/html",Www_Root & "/page.html");
    end if;
  end Hw_Cb;

  --------------
  -- On_Close --
  --------------

  overriding procedure On_Close (Socket : in out Object; Message : String) is
  begin
    Text_Io.Put_Line ("On_Close : " & Message);
    Text_Io.Put_Line ("On_Close : " & Socket.To_String);

    if Aws.Status.Has_Session(Socket.Request) then
      Text_Io.Put_Line ("On_Close has session: True");
      Text_Io.Put_Line ("On_Close session: " & AWS.Session.Image(Aws.Status.Session(Socket.Request)));
    else
      Text_Io.Put_Line ("On_Close has session: False");
    end if;


    Notification_Center.Protected_Center.Unsubscribe (Socket);
  end On_Close;

  --------------
  -- On_Error --
  --------------

  overriding procedure On_Error (Socket : in out Object; Message : String) is
  begin
    Text_Io.Put_Line ("On_Error : " & Message);
    Text_Io.Put_Line ("On_Error : " & Socket.To_String);

    if Aws.Status.Has_Session(Socket.Request) then
      Text_Io.Put_Line ("On_Error has session: True");
      Text_Io.Put_Line ("On_Error session: " & AWS.Session.Image(Aws.Status.Session(Socket.Request)));
    else
      Text_Io.Put_Line ("On_Error has session: False");
    end if;

    Notification_Center.Protected_Center.Unsubscribe (Socket);
  end On_Error;

  ----------------
  -- On_Message --
  ----------------

  overriding procedure On_Message
    (Socket : in out Object; Message : String)
  is
    Comma_Index : constant Natural := Strings.Fixed.Index (Message, ",");
  begin
    Text_Io.Put_Line ("On_Message : " & Message);
    Text_Io.Put_Line ("On_Message : " & Socket.To_String);

    if Aws.Status.Has_Session(Socket.Request) then
      Text_Io.Put_Line ("On_Message has session: True");
      Text_Io.Put_Line ("On_Message session: " & AWS.Session.Image(Aws.Status.Session(Socket.Request)));
    else
      Text_Io.Put_Line ("On_Message has session: False");
    end if;




    if Comma_Index /= 0 then
      declare
        Cmd   : constant String :=
                  Message (Message'First .. Comma_Index-1);
        Param : constant String :=
                  Message (Comma_Index+1 .. Message'Last);
      begin
        if Cmd = "subscribe" then
          Notification_Center.Protected_Center.Subscribe (Socket, Param);
        elsif Cmd = "unsubscribe" then
          Notification_Center.
            Protected_Center.Unsubscribe (Socket, Param);
        end if;
      end;
    else
      if Message = "close" then
        Socket.Shutdown;
      end if;
    end if;

  end On_Message;

  -------------
  -- On_Open --
  -------------

  overriding procedure On_Open (Socket : in out Object; Message : String) is
  begin
    Text_Io.Put_Line ("On_Open : " & Message);
    Text_Io.Put_Line ("On_Open : " & Socket.To_String);
  end On_Open;

  -----------
  -- W_Log --
  -----------

  procedure W_Log
    (Direction : Net.Log.Data_Direction;
     Socket    : Net.Socket_Type'Class;
     Data      : Stream_Element_Array;
     Last      : Stream_Element_Offset)
  is
    Max : constant := 6;
    Str : String (1 .. Max);
    I   : Natural := Str'First - 1;
  begin
    Text_Io.Put_Line (Net.Log.Data_Direction'Image (Direction));
    Text_Io.Put_Line ("[");

    for K in Data'First .. Last loop
      I := I + 1;
      if Characters.Handling.Is_Graphic (Character'Val (Data (K))) then
        Str (I) := Character'Val (Data (K));
      else
        Str (I) := '.';
      end if;

      Text_Io.Put (Str (I));

      Text_Io.Put ('|');
      Integer_Text_Io.Put (Integer (Data (K)), Base => 16, Width => 6);
      Text_Io.Put ("   ");

      if K mod Max = 0 then
        Text_Io.Put_Line (" " & Str (Str'First .. I));
        I := Str'First - 1;
      end if;
    end loop;

    if I > Str'First then
      Text_Io.Set_Col (67);
      Text_Io.Put_Line (" " & Str (Str'First .. I));
    end if;

    Text_Io.Put_Line ("]");
  end W_Log;


 function To_String(Socket : Object) return String is  --bnl
    Ubs : Unbounded_String;
  begin

    --Append(Ubs, "Id: " & Socket.Id'Img);
    if Aws.Status.Has_Session(Socket.Request) then
      Append(Ubs, "| has session: True");
      Append(Ubs, "| session: " & AWS.Session.Image(aws.Status.Session(Socket.Request)));
    else
      Append(Ubs, "| has session: False");
    end if;
    Append(Ubs, "| Version: " & Socket.Protocol_Version'Img);
    Append(Ubs, "| C: " & Socket.C'Img);
    Append(Ubs, "| Origin: " &    Aws.Status.Origin(Socket.Request));
    Append(Ubs, "| Peername: " &  Aws.Status.Peername(Socket.Request));
    Append(Ubs, "| UID: " & Socket.Get_Uid'img);
    Append(Ubs, "| Peer_Addr: " & Socket.Peer_Addr);
    Append(Ubs, "| Peer_Port:" & Socket.Peer_Port'img);


  --  Append(Ubs, "| Origin: " &    Aws.Status.Origin(Socket.Request));

    return To_String(Ubs);
  end To_String;


end Websock_Cb;
