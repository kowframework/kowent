

with Ada.Text_IO;


with APQ;
with APQ_Provider.MySQL;

with KOW_Config;
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Data_Storages;
with KOW_Ent.DB;
with KOW_Ent.DB.Data_Storages;
with KOW_Ent.Extra_Properties;
with KOW_Ent.Properties;
with KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;
with KOW_Lib.String_Util;




with users;
use users;


procedure Mass_Insert is



	procedure Insert is
		U : User_Entity;
	begin
		KOW_Lib.String_Util.Copy( From => "Marcelo", To => U.Name.Value.String_Value );
		KOW_Ent.Extra_Properties.Set_Password( U.Passwd, "x" );
		Store( u );
	end Insert;

	task type insert_task;
	task body insert_Task is
	begin
		for i in 1 .. 1_000 loop
			Insert;
		--	delay 0.1;
		end loop;
	end insert_Task;

	type insert_ptr is access insert_Task;

	t: insert_ptr;
	
begin
	KOW_Config.Add_Config_Path( "." );
	KOW_Ent.DB.Setup;

	Ada.Text_IO.Put_Line( "Inserting tons of data... this will take some time (starting in 1 sec)" );

	delay 1.0;

	for i in 1 .. 10 loop
		T := new Insert_Task;
	end loop;

end Mass_Insert;
