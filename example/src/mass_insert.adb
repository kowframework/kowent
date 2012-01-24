

with Ada.Text_IO;


with APQ;
with APQ_Provider.MySQL;

with KOW_Config;
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Data_Storages;
with KOW_Ent.DB;
with KOW_Ent.DB.Data_Storages;
with KOW_Ent.Properties;
with KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;
with KOW_Lib.String_Util;




with users;
use users;


procedure Mass_Insert is
begin
	KOW_Config.Add_Config_Path( "." );
	KOW_Ent.DB.Setup;

	Ada.Text_IO.Put_Line( "Inserting tons of data... this will take some time (starting in 1 sec)" );
	delay 1.0;

	for i in 1 .. 10_000_000 loop
		declare
			U : User_Entity;
		begin
			KOW_Lib.String_Util.Copy( From => "Marcelo", To => U.Name.Value.String_Value );
			Store( u );
		end; 
	end loop;

end Mass_Insert;
