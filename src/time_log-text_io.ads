with Ada.Text_IO;

package Time_Log.Text_IO is
   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Line);
   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Line);

   procedure Get_Day (File        : in     Ada.Text_IO.File_Type;
                      First_Line  : in     Line;
                      Tags        :    out Tag_Sets.Set;
                      Tasks       :    out Task_Maps.Map;
                      End_Of_File :    out Boolean;
                      Last_Line   :    out Line);

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Comment_Sets.Set);
end Time_Log.Text_IO;
