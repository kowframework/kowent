


package body Users is
	procedure Put( Usr : User_Entity ) is
	begin
		Ada.Text_IO.Put_line( "     * " & APQ.APQ_Bigserial'Image( Usr.Id.Value.Bigserial_Value ) & " => " & Usr.Name.Value.String_Value );
	end Put;
end Users;
