

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
with KOW_Lib.Locales;
with KOW_Lib.String_Util;




with users;
use users;



procedure Example is
	
	U : User_Entity;

	First_Job : Boolean := True;

	procedure new_job is
		use KOW_Ent.Properties;
		J : Job_Entity;
	begin
		if First_Job then
			KOW_Lib.String_Util.Copy( From => "Job 1", To => J.Title.Value.String_Value );
			First_Job := False;
		else
			KOW_Lib.String_Util.Copy( From => "Job 2", To => J.Title.Value.String_Value );
		end if;

		Set_Value( J.User_Id, Get_Value( U.Id ) );
		Store( J );
	end new_job;
begin
	KOW_Config.Add_Config_Path( "." );
	KOW_Ent.DB.Setup;

	KOW_Lib.String_Util.Copy( From => "Marcelo", To => U.Name.Value.String_Value );
	Store( U );
	Put( u );

	KOW_Lib.String_Util.Copy( From => "Marcelo 2", To => U.Name.Value.String_Value );
	KOW_Ent.Extra_Properties.Set_Password( U.Passwd, "OMFG They killed kenny!" );
	KOW_Ent.Extra_Properties.Set_Locale( U.Locale, KOW_Lib.Locales.Get_Locale( "en_US" ) );

	Store( U );
	Put( u );



	new_job;
	new_job;


	-- now we select some values..
	

	-- A simple Query
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



	-- A join query
	Ada.Text_IO.New_Line(2);
	declare
		use KOW_Ent.Queries;
		use KOW_Ent.Queries.Logic_Relations;
		use KOW_Ent.Data_Storages;


		Q		: Query_Type;
		JQ		: KOW_Ent.Queries.Join_Query_Type;
		Op		: Stored_Vs_Stored_Operation;
		Condition	: Logic_Criteria_Type;
	begin

		------------------------------
		-- Setup the Join Condition --
		------------------------------
		Op.Left_Entity_Tag := User_Entity'Tag;
		Op.Left_Property_Name := Id_Name;
		
		Op.Right_Entity_Tag := Job_Entity'Tag;
		Op.Right_Property_Name := User_Id_Name;
		Append( Condition, Op );


		-------------------------
		-- Setup the sub query --
		-------------------------
		Q.Entity_Tag := Job_Entity'Tag;
		Append( JQ, ( Query => Q, Condition => Condition, Join => Right_Join ) );
		-- this way we'll only get users with jobs... :)

		declare
			use APQ;
			Loader : Entity_Loader_Interface'Class := New_Loader( Data_Storage_Type'Class( Get_Data_Storage( User_Entity'Tag ).all ), JQ );
			U : User_Entity;
			J : Job_Entity;

			Last_ID : APQ.APQ_Bigserial;
		begin
			Execute( Loader );
			loop
				Fetch( Loader );
				exit when not Has_Element( Loader );
				Load( Loader, U );
				if Last_ID /= U.ID.Value.Bigserial_Value then
					Last_ID := U.ID.Value.Bigserial_Value;
					Put( U );
				end if;
				Load( Loader, J );
				Ada.Text_IO.Put_line( "         => " & J.Title.Value.String_Value );
			end loop;
		end;
	end;
end Example;

