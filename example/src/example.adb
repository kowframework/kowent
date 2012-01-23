




with KOW_Ent;			use KOW_Ent;
with KOW_Ent.DB;
with KOW_Ent.DB.Data_Storages;
with KOW_Ent.Properties;
with KOW_Lib.String_Util;


procedure Example is


	Id_Name		: constant KOW_Ent.Property_Name_Type := new String'( "user_id" );
	Name_Name	: constant KOW_Ent.Property_Name_Type := new String'( "name" );


	type User_Entity is new KOW_Ent.Entity_Type with record
		ID	: KOW_Ent.Properties.Id_Property(
							Name		=> Id_Name,
							Container	=> User_Entity'Unrestricted_Access
					);
		Name	: KOW_Ent.Properties.String_Property(
							Name		=> Name_Name,
							Container	=> User_Entity'Unrestricted_Access,
							String_Length	=> 20
						);
	end record;


	package User_Storages is new KOW_Ent.DB.Data_Storages(
						Entity_Type	=> User_Entity,
						Entity_Alias	=> "users"
					);

	U : User_Entity;
begin

	KOW_Lib.String_Util.Copy( From => "Marcelo", To => U.Name.Value.String_Value );

	Store( U );

	KOW_Lib.String_Util.Copy( From => "Marcelo 2", To => U.Name.Value.String_Value );

	Store( U );


	KOW_Ent.DB.Setup;
end Example;
