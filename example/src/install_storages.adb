

with Ada.Text_IO;


with APQ;
with APQ_Provider.MySQL;

with KOW_Config;
with KOW_Ent.Data_Storages;
with KOW_Ent.DB;
with KOW_Ent.DB.Data_Storages;


with users;
pragma Elaborate( Users );


procedure Install_Storages is
begin
	KOW_Config.Add_Config_Path( "." );
	KOW_Ent.DB.Setup;

	KOW_Ent.Data_Storages.Install;
	-- this will create all the tables
end Install_Storages;
