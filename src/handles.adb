package body Handles is
   function Activate return Active_Handle is
   begin
      return (V => 1);
   end Activate;
end Handles;
