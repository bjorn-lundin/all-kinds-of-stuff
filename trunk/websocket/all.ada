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

with Ada.Streams;

with AWS.Net.Log;
with AWS.Response;
with AWS.Status;

with AWS.Net.WebSocket;

package WebSock_CB is

   use Ada.Streams;
   use AWS;

   function HW_CB (Request : Status.Data) return Response.Data;

   procedure W_log
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset);

   --  My WebSocket, just display the messages

   type Object is new AWS.Net.WebSocket.Object with private;

   function Create
     (Socket  : Net.Socket_Access;
      Request : Status.Data) return Net.WebSocket.Object'Class;

   overriding procedure On_Message (Socket : in out Object; Message : String);
   --  Message received from the server

   overriding procedure On_Open (Socket : in out Object; Message : String);
   --  Open event received from the server

   overriding procedure On_Close (Socket : in out Object; Message : String);
   --  Close event received from the server

   overriding procedure On_Error (Socket : in out Object; Message : String);

private

   type Object is new Net.WebSocket.Object with record
      C : Natural := 0;
   end record;

end WebSock_CB;


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
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Messages;
with AWS.MIME;
with AWS.Templates;
with AWS.Translator;

with GNAT.RegPat;

with Notification_Center;

package body WebSock_CB is

   use Ada;
   use type AWS.Net.WebSocket.Kind_Type;

   WWW_Root : constant String := "../../web_elements";

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : Net.Socket_Access;
      Request : Status.Data) return Net.WebSocket.Object'Class is
   begin
      return Object'(Net.WebSocket.Object
                       (Net.WebSocket.Create (Socket, Request)) with C => 0);
   end Create;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      if URI'Length > 6
        and then URI (URI'First .. URI'First + 6) = "/we_js/"
      then
         return AWS.Response.Build
           (MIME.Text_Javascript,
            Message_Body => Templates.Parse
              (WWW_Root & "/javascripts" & URI (URI'First + 6 .. URI'Last)));

      elsif URI'Length = 12
        and then URI (URI'First .. URI'First + 11) = "/favicon.ico"
      then
         return AWS.Response.Acknowledge (Messages.S404);

      else
         return Response.Build
           ("text/html", String'(Templates.Parse ("page.thtml")));
      end if;
   end HW_CB;

   --------------
   -- On_Close --
   --------------

   overriding procedure On_Close (Socket : in out Object; Message : String) is
   begin
      Notification_Center.Protected_Center.Unsubscribe (Socket);
   end On_Close;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (Socket : in out Object; Message : String) is
   begin
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
      Text_IO.Put_Line ("Received : " & Message);

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
      null;
   end On_Open;

   -----------
   -- W_Log --
   -----------

   procedure W_log
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset)
   is
      Max : constant := 6;
      Str : String (1 .. Max);
      I   : Natural := Str'First - 1;
   begin
      Text_IO.Put_Line (Net.Log.Data_Direction'Image (Direction));
      Text_IO.Put_Line ("[");

      for K in Data'First .. Last loop
         I := I + 1;
         if Characters.Handling.Is_Graphic (Character'Val (Data (K))) then
            Str (I) := Character'Val (Data (K));
         else
            Str (I) := '.';
         end if;

         Text_IO.Put (Str (I));

         Text_IO.Put ('|');
         Integer_Text_IO.Put (Integer (Data (K)), Base => 16, Width => 6);
         Text_IO.Put ("   ");

         if K mod Max = 0 then
            Text_IO.Put_Line (" " & Str (Str'First .. I));
            I := Str'First - 1;
         end if;
      end loop;

      if I > Str'First then
         Text_IO.Set_Col (67);
         Text_IO.Put_Line (" " & Str (Str'First .. I));
      end if;

      Text_IO.Put_Line ("]");
   end W_Log;

end WebSock_CB;

------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WebSock_CB;

package Notification_Center is

   use Ada.Strings.Unbounded;

   type Subscription is record
      Socket : WebSock_CB.Object;
      Key    : Unbounded_String;
   end record;

   package Subscription_Vectors is
      new Ada.Containers.Vectors (Natural, Subscription);

   protected Protected_Center is

      procedure Subscribe (Socket : WebSock_CB.Object; Key : String);

      procedure Unsubscribe (Socket : WebSock_CB.Object; Key : String);

      procedure Unsubscribe (Socket : WebSock_CB.Object);

      procedure Notify (Key : String);

      function Get_Subscriptions_For_Key
        (Key : String) return Subscription_Vectors.Vector;

   private
      Subscriptions : Subscription_Vectors.Vector;
   end Protected_Center;

   procedure Unprotected_Notify (Key : String);

end Notification_Center;


------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Text_IO;

with AWS.Net.WebSocket.Registry;

package body Notification_Center is

   Padding : String (1 .. 64_000) := (others => 'a');
   --  Padding to encourage threading issues

   protected body Protected_Center is

      ---------------
      -- Subscribe --
      ---------------

      procedure Subscribe (Socket : WebSock_CB.Object; Key : String) is
         S : constant Subscription :=
               (Socket => Socket, Key => To_Unbounded_String (Key));
      begin
         Subscriptions.Append (S);
      end Subscribe;

      -----------------
      -- Unsubscribe --
      -----------------

      procedure Unsubscribe (Socket : WebSock_CB.Object; Key : String) is
         use type WebSock_CB.Object;
         New_Subscriptions : Subscription_Vectors.Vector;
      begin
         for S of Subscriptions loop
            if S.Socket /= Socket
              or else S.Key /= To_Unbounded_String (Key)
            then
               New_Subscriptions.Append (S);
            end if;
         end loop;
         Subscriptions := New_Subscriptions;
      end Unsubscribe;

      procedure Unsubscribe (Socket : WebSock_CB.Object) is
         use type WebSock_CB.Object;
         New_Subscriptions : Subscription_Vectors.Vector;
      begin
         for S of Subscriptions loop
            if S.Socket /= Socket then
               New_Subscriptions.Append (S);
            end if;
         end loop;
         Subscriptions := New_Subscriptions;
      end Unsubscribe;

      ------------
      -- Notify --
      ------------

      procedure Notify (Key : String) is
         Message : constant String := Key & "," & Padding;
      begin
         for S of Subscriptions loop
            if To_String (S.Key) = Key then
               AWS.Net.WebSocket.Registry.Send (S.Socket, Message);
            end if;
         end loop;
      end Notify;

      -------------------------------
      -- Get_Subscriptions_For_Key --
      -------------------------------

      function Get_Subscriptions_For_Key
        (Key : String) return Subscription_Vectors.Vector
      is
         New_Subscriptions : Subscription_Vectors.Vector;
      begin
         for S of Subscriptions loop
            if To_String (S.Key) = Key then
               New_Subscriptions.Append (S);
            end if;
         end loop;
         return New_Subscriptions;
      end Get_Subscriptions_For_Key;

   end Protected_Center;

   ------------------------
   -- Unprotected_Notify --
   ------------------------

   --  Occurs outside of the protected block, so we could be executing Send
   --  at the same time as other tasks.

   procedure Unprotected_Notify (Key : String) is
      Message       : constant String := Key & "," & Padding;
      Subscriptions : Subscription_Vectors.Vector :=
                        Protected_Center.Get_Subscriptions_For_Key (Key);
   begin
      for S of Subscriptions loop
         AWS.Net.WebSocket.Registry.Send (S.Socket, Message);
      end loop;
   end Unprotected_Notify;

end Notification_Center;


------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

package Candy_Mine is

   task type Candy_Miner is
      entry Start (Key : String);
   end Candy_Miner;

end Candy_Mine;


------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Notification_Center;

package body Candy_Mine is

   use Ada.Strings.Unbounded;

   -----------------
   -- Candy_Miner --
   -----------------

   task body Candy_Miner is
      Key_US : Unbounded_String;
   begin
      accept Start (Key : String) do
         Key_US := To_Unbounded_String (Key);
      end Start;

      loop
         delay 0.1;
         --  Notification_Center.Protected_Center.Notify (To_String (Key_US));
         Notification_Center.Unprotected_Notify (To_String (Key_US));
      end loop;
   end Candy_Miner;

end Candy_Mine;


------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

--  A simple WebSocket demo using AWS framework

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Default;
with AWS.Net.Log;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Server;
with AWS.Status;
with AWS.Templates;

with WebSock_CB;

with Candy_Mine;
with Notification_Center;

procedure Candy is
   use Ada;
   use AWS;
   use AWS.Config;
   use type AWS.Net.Socket_Access;

   WS           : Server.HTTP;
   Config       : AWS.Config.Object;

   Candy_Miners : array (1 .. 15) of Candy_Mine.Candy_Miner;

begin
   --  To analyse the send/received data uncomment the line below
   --  Net.Log.Start (WebSock_CB.W_Log'Access);

   Candy_Miners (1).Start ("Candy");
   Candy_Miners (2).Start ("Lollipop");
   Candy_Miners (3).Start ("Snickers");
   Candy_Miners (4).Start ("Bubble Tape");
   Candy_Miners (5).Start ("Gummi Bears");
   Candy_Miners (6).Start ("Corn Candy");
   Candy_Miners (7).Start ("Cherry Ripe");
   Candy_Miners (8).Start ("Chocolate");
   Candy_Miners (9).Start ("Skittles");
   Candy_Miners (10).Start ("Bullets");
   Candy_Miners (11).Start ("Minties");
   Candy_Miners (12).Start ("Kool Mints");
   Candy_Miners (13).Start ("Jaffas");
   Candy_Miners (14).Start ("Fantales");
   Candy_Miners (15).Start ("Clinkers");

   AWS.Config.Set.Reuse_Address (Config, True);
   Net.WebSocket.Registry.Control.Start;
   Net.WebSocket.Registry.Register ("/candy", WebSock_CB.Create'Access);

   Server.Start (WS, Config => Config, Callback => WebSock_CB.HW_CB'Access);

   Text_IO.Put_Line
     ("Call me on port" & Positive'Image (AWS.Default.Server_Port));
   Text_IO.Put_Line ("You can now press Q to exit.");
   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Candy;


