

with Ada.Text_IO;
with Ada.COmmand_Line;
use Ada.Command_Line;


with APQ;
with APQ_Provider.MySQL;
with APQ_Provider.PostgreSQL;

with KOW_Config;
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Data_Storages;
with KOW_Ent.DB;
with KOW_Ent.DB.Data_Storages;
with KOW_Ent.Properties;
with KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;
with KOW_Lib.String_Util;


use KOW_Ent.Queries;
use KOW_Ent.Data_Storages;


with users;
use users;


procedure Mass_Query is
	Q : query_Type;
begin

	KOW_Config.Add_Config_Path( "." );
	KOW_Ent.DB.Setup;

	if Argument_Count >= 1 then
		declare
			use KOW_Ent.Queries.Logic_Relations;
			Op	: Stored_Vs_Value_Operation;

			function To_Value( Str : in String ) return Value_Type is
				Value	: Value_Type( APQ_String, Str'Length );
			begin
				Value.String_Value := Str;
				return Value;
			end To_Value;
		begin
			Op.Property_Name := Name_Name;
			Op.Value := new Value_Type'( To_Value( Argument( 1 ) ) );
			Op.Relation := Relation_Like;

			Append( Q.Logic_Criteria, Op );
		end;
	end if;

	declare
		Loader : Entity_Loader_Interface'Class := New_Loader( Data_Storage_Type'Class( Get_Data_Storage( User_Entity'Tag ).all ), Q );
		Usr : User_Entity;
	begin
		Execute( Loader );

		loop
			Fetch( Loader );
			exit when not Has_Element( Loader );
			Load( Loader, Usr );
			Put( Usr );
		end loop;
	end;
end Mass_Query;

