




with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;


procedure Example is


	Id_Name		: constant KOW_Ent.Property_Alias_Type := new String'( "user_id" );
	Name_Name	: constant KOW_Ent.Property_Alias_Type := new String'( "name" );


	type User_Entity is new KOW_Ent.Entity_Type with record
		ID	: Properties.Id_Property(
							Name		=> Id_Name,
							Container	=> User_Entity'Unrestricted_Access
					);
		Name	: Properties.String_Property(
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

	KOW_Lib.String_Util.Copy( From => "Marcelo", To => U.Name.String_Value );

	User_Storages.Insert( U );


	KOW_Lib.String_Util.Copy( From => "Marcelo 2", To => U.Name.String_Value );

	User_Storages.Update( U );


	KOW_Ent.Setup;
end Example;
