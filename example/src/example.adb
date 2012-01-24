

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



procedure Example is


	Id_Name		: constant KOW_Ent.Property_Name_Type := new String'( "id" );
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



	procedure Put( Usr : User_Entity ) is
	begin
		Ada.Text_IO.Put_line( "     * " & APQ.APQ_Bigserial'Image( Usr.Id.Value.Bigserial_Value ) & " => " & Usr.Name.Value.String_Value );
	end Put;


	U : User_Entity;
begin
	KOW_Config.Add_Config_Path( "." );
	KOW_Ent.DB.Setup;

	KOW_Lib.String_Util.Copy( From => "Marcelo", To => U.Name.Value.String_Value );
	Store( U );
	Put( u );

	KOW_Lib.String_Util.Copy( From => "Marcelo 2", To => U.Name.Value.String_Value );

	Store( U );
	Put( u );



	-- now we select some values..
	

	Ada.Text_IO.New_Line(2);
	declare
		use KOW_Ent.Queries;
		use KOW_Ent.Queries.Logic_Relations;
		use KOW_Ent.Data_Storages;


	
		Q	: Query_Type;
		Op	: Stored_Vs_Value_Operation;

		function To_Value( Str : in String ) return Value_Type is
			Value	: Value_Type( APQ_String, Str'Length );
		begin
			Value.String_Value := Str;
			return Value;
		end To_Value;
	begin
		Op.Property_Name := Name_Name;
		Op.Value := new Value_Type'( To_Value( "%2" ) );
		Op.Relation := Relation_Like;

		Append( Q.Logic_Criteria, Op );

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
	end;
end Example;

