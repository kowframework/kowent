


--------------
-- Ada 2005 --
--------------
with Ada.Text_IO;

-------------
-- KOW Ent --
-------------
with KOW_Ent;
with KOW_Ent.Properties;


with APQ;


procedure properties is

	type My_Container is new KOW_Ent.Entity_Container_Type with record
		My_Int : KOW_Ent.Properties.Integer_Property( "my_int", My_Container'Unrestricted_Access );
		My_Real : KOW_Ent.Properties.Real_Property( "my_real", My_Container'Unrestricted_Access );
	end record;

	procedure Print_Properties( My: in My_Container ) is
		use KOW_Ent.Property_Lists;

		procedure Iterator( C : Cursor ) is
			E : Property_Ptr := Element( C );
		begin
			Put_Line( E.Name & " => " & KOW_Ent.To_String( E.all ) );
		end Iterator;
	begin
		New_Line;
		Iterate( My.Properties, Iterator'Access );
		New_Line;
	end Print_Properties;

	My : My_Container;
begin

	Put_Properties( My );

	My.My_Int.Integer_Value := 20;
	My.My_Real.Real_Value := 12.0;

	Put_Properties( My );

end properties;
