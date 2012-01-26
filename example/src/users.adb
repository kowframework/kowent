


package body Users is


	overriding
	procedure Pos_Install(
				User		: in out User_Entity;
				Data_Storage	: in out KOW_Ent.Data_Storage_Interface'Class
			) is
		-- create the name/lastname index
	begin

		KOW_Ent.Create_Index(
					Data_Storage	=> Data_Storage,
					Entity_Tag	=> User_Entity'Tag,
					Property_Names	=> ( 1 => Name_Name, 2 => Lastname_name ),
					Is_Unique	=> False
				);

	end Pos_Install;

	procedure Put( Usr : User_Entity ) is
	begin
		Ada.Text_IO.Put_line( "     * " & APQ.APQ_Bigserial'Image( Usr.Id.Value.Bigserial_Value ) & " => " & Usr.Name.Value.String_Value );
	end Put;



	overriding
	procedure Pos_Install(
				User		: in out Job_Entity;
				Data_Storage	: in out KOW_Ent.Data_Storage_Interface'Class
			) is
		-- create the title index
	begin
		KOW_Ent.Create_Index(
					Data_Storage	=> Data_Storage,
					Entity_Tag	=> Job_Entity'Tag,
					Property_Names	=> ( 1 => Title_name ),
					Is_Unique	=> True
				);
	end Pos_Install;

end Users;
