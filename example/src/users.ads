

with Ada.Text_IO;


with APQ;
with APQ_Provider.MySQL;
with APQ_Provider.PostgreSQL;

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



package Users is


	-----------------
	-- User Entity --
	-----------------

	Id_Name		: constant KOW_Ent.Property_Name_Type := new String'( "id" );
	Name_Name	: constant KOW_Ent.Property_Name_Type := new String'( "name" );
	Lastname_Name	: constant KOW_Ent.Property_Name_Type := new String'( "lastname" );
	Password_Name	: constant KOW_Ent.Property_Name_Type := new String'( "password" );
	Locale_Name	: constant KOW_Ent.Property_Name_Type := new String'( "locale" );


	type User_Entity is new KOW_Ent.Entity_Type with record
		ID	: KOW_Ent.Properties.Id_Property(
							Name		=> Id_Name,
							Container	=> User_Entity'Unrestricted_Access
					);
		Name	: KOW_Ent.Properties.String_Property(
							Name		=> Name_Name,
							Container	=> User_Entity'Unrestricted_Access,
							String_Length	=> 20,
							Allow_Null	=> True
						);

		Lastname: KOW_Ent.Properties.String_Property(
							Name		=> Lastname_Name,
							Container	=> User_Entity'Unrestricted_Access,
							String_Length	=> 20,
							Allow_Null	=> True
						);



		Passwd	: KOW_Ent.Extra_Properties.Password_Property_Type(
							Name		=> Password_Name,
							Container	=> User_Entity'Unrestricted_Access
						);

		Locale	: KOW_Ent.Extra_Properties.Locale_Property_Type(
							Name		=> Locale_Name,
							Container	=> User_Entity'Unrestricted_Access
						);
	end record;

	overriding
	procedure Post_Install(
				User		: in out User_Entity;
				Data_Storage	: in out KOW_Ent.Data_Storage_Interface'Class
			);
	-- create the name/lastname index

	procedure Put( Usr : User_Entity );


	package User_Storages is new KOW_Ent.DB.Data_Storages(
						Entity_Type	=> User_Entity,
						Entity_Alias	=> "users"
					);



	----------------
	-- Job Entity --
	----------------

	User_Id_Name	: constant KOW_Ent.Property_Name_Type := new String'( "user_id" );
	Title_Name	: constant KOW_Ent.Property_Name_Type := new String'( "title" );
	
	type Job_Entity is new KOW_Ent.Entity_Type with record
		ID	: KOW_Ent.Properties.Id_Property(
							Name		=> Id_Name,
							Container	=> Job_Entity'Unrestricted_Access
						);
		User_Id	: KOW_Ent.Properties.Bigserial_Property(
							Name		=> User_ID_Name,
							COntainer	=> Job_Entity'Unrestricted_Access,
							Allow_Null	=> True
						);
		Title	: KOW_Ent.Properties.String_Property(
							Name		=> Title_Name,
							Container	=> Job_Entity'Unrestricted_Access,
							String_Length	=> 100,
							Allow_Null	=> True
						);
	end record;

	overriding
	procedure Post_Install(
				User		: in out Job_Entity;
				Data_Storage	: in out KOW_Ent.Data_Storage_Interface'Class
			);
	-- create the title index

	package Job_Storages is new KOW_Ent.DB.Data_Storages(
						Entity_Type	=> Job_Entity,
						Entity_Alias	=> "jobs"
					);




end Users;
