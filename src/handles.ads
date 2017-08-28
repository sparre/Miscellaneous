package Handles is
   type Handle_Type is private;
   subtype Active_Handle is Handle_Type
     with Dynamic_Predicate => Is_Active (Active_Handle);

   function Activate return Active_Handle;

   procedure Do_Something (H : in Active_Handle) is null;

   function Is_Active (H : in Handle_Type) return Boolean;
private
   type Handle_Type is
      record
         V : Natural := 0;
      end record;

   function Is_Active (H : in Handle_Type) return Boolean is (H.V > 0);
end Handles;
