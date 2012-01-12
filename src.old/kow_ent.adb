------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWLib is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWLib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWLib; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Log;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;

with KOW_Ent.Properties;


--------------------------------------
-- The password is now a SHA1 Hash! --
--------------------------------------
with GNAT.SHA1;

package body KOW_Ent is

	
	Logger : KOW_Lib.Log.Logger_Type := 
			KOW_Lib.Log.Get_Logger( "KOW_Ent" );
	

	procedure Log(
			Message : in String;
			Level : KOW_Lib.Log.Log_Level := KOW_lib.Log.Level_Info
		) is
	begin
		KOW_lib.Log.Log(
				Logger	=> Logger,
				Level	=> Level,
				Message	=> Message
			);
	end Log;



	----------------------
	-- Auxiliar Methods --
	----------------------

	type Id_Array_Ptr is access Id_Array_Type;
	procedure Free is new Ada.Unchecked_Deallocation(
				Name	=> Id_Array_Ptr,
				Object	=> Id_Array_Type
			);


	function To_UString( Str : in String ) return Unbounded_String is
	begin
		return Ada.Strings.Unbounded.To_Unbounded_String(
				Ada.Characters.Handling.To_Lower(
					Ada.Strings.Fixed.Trim(
							Str,
							Ada.Strings.Both
						)
					)
				);
	end To_UString;


	function To_UString( T : in Ada.Tags.Tag ) return Unbounded_String is
		Str : String := Ada.Tags.Expanded_Name( T );
	begin
		return To_UString( Str );
	end To_UString;

	function To_UString( U : in Ada.Strings.Unbounded.Unbounded_String ) return Unbounded_String is
		Str : String := Ada.Strings.Unbounded.To_String( U );
	begin

		return To_UString( Str );
	end To_UString;


	procedure Run_And_Fetch(
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class
			) is
		-- when we run a query, we gotta fetch all results..
		-- or the connection will be trashed..
	begin
		APQ.Execute( Query, Connection );

		begin
			loop
				APQ.Fetch( Query );
			end loop;
		exception
			when others => null;
		end;
	end Run_And_Fetch;





	procedure Prepend_All( Container : in out Property_Lists.List; Source : in Property_Lists.List ) is
		-- add all elements in Source before all elements in Container inside


		procedure Iterator( C : in Property_Lists.Cursor ) is
		begin
			Property_Lists.Prepend( Container, Property_Lists.Element( C ) );
		end Iterator;
	begin
		Property_Lists.Reverse_Iterate( Source, Iterator'Access );
	end Prepend_All;




	-----------
	-- Setup --
	-----------
	procedure Setup is
		-- looks for the kow_ent configuration file and initializes the provider
		Provider : APQ_Provider.Connection_Provider_Ptr := new APQ_Provider.Connection_Provider_Type;
	begin
		Provider.Setup( KOW_Config.New_Config_File( "kow_ent" ) );
		KOW_Ent.Set_Connection_Provider( Provider );
	end Setup;

	-------------------------
	-- Database Management --
	-------------------------
	procedure Set_Connection_Provider( Provider : in APQ_Provider.Connection_Provider_Ptr ) is
		-- set the current database connection provider
	begin
		My_Provider := Provider;
	end Set_Connection_Provider;


	function Get_Connection_Provider return APQ_Provider.Connection_Provider_Ptr is
		-- get the current database connection provider
	begin
		return My_Provider;
	end Get_Connection_Provider;

	function To_String( ID: in Id_Type ) return String is
		-- get the ID value as a String
	begin
		-- TODO: I know there is a better way of doing it but I just can't remember how
		return Ada.Strings.Fixed.Trim( APQ.APQ_Bigserial'Image( ID.Value ), Ada.Strings.Both );
	end To_String;



	-------------------------
	-- Password Management --
	-------------------------
	function Calculate_Hash( Pwd : in String ) return String is
		-- return a hashed version of Pwd.
		-- Used in both KOW_Ent.Properties and KOW_Ent.Query_Builders for handling password fields
		use GNAT.SHA1;
		C : Context;
	begin
		Update( C, Pwd );
		return Digest( C );
	end Calculate_Hash;



	-------------------
	-- ID Management --
	-------------------
	

	function "<"( L, R : in ID_Type ) return Boolean is
		use APQ;
	begin
		return L.Value < R.Value;
	end "<";

	function ID_Value( Query : APQ.Root_Query_Type'Class; CX : APQ.Column_Index_Type) return APQ.APQ_Bigserial is
		-- get the ID value from a query
		i : Integer := APQ.Value( Query, CX );
	begin
		return APQ.APQ_Bigserial( i );
	end ID_Value;



	------------------------
	-- Query All Elements --
	------------------------
	function Get_All_IDs( Entity_Tag : Ada.Tags.Tag ) return ID_Array_Type is
		-- get all IDs from a given entity

		My_Ids : Id_Array_Ptr;

		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
			Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity_Tag );


			function Inner_Get_All_Ids return ID_Array_Type is
				Ret : ID_Array_Type( 1 .. 1 );
				TMP_Id : Integer;
				ID : ID_Type;
				
			begin
				APQ.Fetch( Query );
	
				TMP_Id := APQ.Value( Query, APQ.Column_Index( Query, "id" ) );
				ID := To_ID( TMP_Id );
				ID.My_Tag := Entity_Tag;
				Ret( 1 ) := ID;
	
				return Ret & Inner_Get_All_Ids;
			exception
				when APQ.No_Tuple => return Ret( 1 .. 0 );
			end Inner_Get_All_Ids;
		begin
			APQ.Prepare( Query, "SELECT id FROM " & To_String( Info.Table_Name ) );
			APQ.Execute( Query, Connection );
	
			declare
				My_Inner_Ids : Id_Array_Type := Inner_Get_All_Ids;
			begin
				My_Ids := new Id_Array_Type'( My_Inner_Ids );
			end;
		end Runner;
	begin
		APQ_Provider.Run( My_Provider.all, Runner'Access );

		declare
			My_Inner_Ids_Again : Id_Array_Type := My_Ids.all;
		begin
			Free( My_Ids );
			return My_Inner_Ids_Again;
		end;
	exception
		when APQ.No_Tuple => 
		declare
			Ret : ID_Array_Type( 1 .. 0 );
		begin
			return Ret;
		end;

	end Get_All_IDs;
	
	function Get_All_IDs( Entity_Tag : Unbounded_String ) return ID_Array_Type is
		-- get all IDs from a given entity

		My_Ids : Id_Array_Ptr;

		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
			Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity_Tag );


			function Inner_Get_All_Ids return ID_Array_Type is
				Ret : ID_Array_Type( 1 .. 1 );
				TMP_Id : Integer;
				ID : ID_Type;
			
			begin
				APQ.Fetch( Query );
	
				TMP_Id := APQ.Value( Query, APQ.Column_Index( Query, "id" ) );
				ID := To_ID( TMP_Id );
				Ret( 1 ) := ID;
	
				return Ret & Inner_Get_All_Ids;
			exception
				when APQ.No_Tuple => return Ret( 1 .. 0 );
			end Inner_Get_All_Ids;
		begin
			APQ.Prepare( Query, "SELECT id FROM " & To_String( Info.Table_Name ) );
			APQ.Execute( Query, Connection );

			My_Ids := new Id_Array_Type'( Inner_Get_All_Ids );
		end Runner;
	begin
		APQ_Provider.Run( My_Provider.all, Runner'Access );

		declare
			My_Inner_Ids : Id_Array_Type := My_Ids.all;
		begin
			Free( My_ids );
			return My_Inner_ids;
		end;
	exception
		when APQ.No_Tuple => 
		declare
			Ret : ID_Array_Type( 1 .. 0 );
		begin
			return Ret;
		end;
	end Get_All_IDs;



	-----------------------
	-- Entity Management --
	-----------------------

	function To_ID( ID: in Natural ) return ID_Type is
		-- convert a positive into an ID.
		-- used for loading entities.
		My_Id : Id_Type;
	begin
		My_ID.Value := APQ.APQ_Bigserial( ID );
		return My_ID;
	end To_ID;
	
	function To_ID( ID: in Natural; Tag : in Ada.Tags.Tag ) return ID_Type is
		-- convert a positive into an ID.
		-- used for loading entities.
		My_Id : Id_Type;
	begin
		My_ID.Value  := APQ.APQ_Bigserial( ID );
		My_ID.My_Tag := Tag;
		return My_ID;
	end To_ID;


	function To_String( Entity : in Entity_Type ) return String is
		-- return the string representation for this entity
		-- as default, return the ID as String.
		-- Should be overriden to something that makes more sence
	begin
		return To_String( Entity.Id );
	end To_String;


	function Describe( Entity : in Entity_Type ) return String is
		-- get a detailed description of this entity...
		-- as default return "Entity represented by """ & To_String( Entity ) & """"
	begin
		return "Entity represented by """ & To_String( Entity ) & """";
	end Describe;

	
	function Image_URL( Entity : in Entity_Type ) return String is
		-- get a URI (http://, https://, file://) with a image file representing the entity
		-- as default return "", representing there is no graphic representation of the given entity
		--
		-- this is quite usefull in gravatar interaction provided by kow_sec-entities package
	begin
		return "";
	end Image_URL;


	function To_Json_Object( Entity : in Entity_Type ) return KOW_Lib.Json.Object_Type is
		-- convert the given entity into json object
		use KOW_Lib.Json;

		Object	: Object_Type;
		E	: Entity_Type'Class := Entity_Type'Class( Entity );

		procedure Iterator( C : Property_Lists.Cursor ) is
			Property : Entity_Property_Ptr := Property_Lists.Element( C );
		begin
			Set( Object, Property.Column_Name, To_Json_Data( Property.all, E ) );
		end Iterator;


		procedure Append_Info( T : in Ada.Tags.Tag ) is
			use Ada.Tags;
			Info : Entity_Information_Type := Get_Information( T );
			PT : Tag := Parent_Tag( T );
		begin
			if PT /= Entity_Type'Tag then
				Append_Info( PT );
			end if;

			Property_Lists.Iterate( Info.Properties, Iterator'Access );
		end Append_Info;
	begin

		Set( Object, "original_tag", Ada.Tags.Expanded_name( E'Tag ) );
		Set( Object, "id", Integer( E.ID.Value ) );
		Set( Object, "filter_tags", E.Filter_Tags );
		Append_Info( E'Tag );

		return Object;
	end To_Json_Object;


	function To_Json_Data (Entity : in Entity_Type ) return KOW_Lib.Json.Json_Data_Type is
		-- convert the given entity into a generic json data (using to_json_object)
	begin
		return KOW_Lib.Json.To_Data( To_Json_Object( Entity ) );
	end To_Json_Data;

	function To_JSon( Entity : in Entity_Type ) return String is
		-- convert the given entity into json string (using to_json_object);
	begin
		return KOW_Lib.Json.To_Json( To_Json_Object( Entity ) );
	end To_JSon;


	function Is_New( Entity : in Entity_Type ) return Boolean is
		-- check if the entity has been stored or not..
		-- tipically using the entity's id's tag value but can be overriden
	begin
		return Entity.ID.My_Tag = No_Tag;
	end Is_New;



	procedure Set_Values_From_Query(
				Entity		: in out Entity_Type'Class;
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class;
				Info		: in     Entity_Information_Type
			) is
		-- set all the values from the resulting query


		procedure Set_Value( C : in Property_Lists.Cursor ) is
		begin
			Set_Property( Property_Lists.Element( C ).all, Entity, Query, Connection );
		end Set_Value;
		TMP_Id : Integer := APQ.Value( Query, APQ.Column_Index( Query, "id" ) );
	begin
		Entity.ID		:= To_ID( TMP_Id );
		Entity.ID.My_Tag	:= Entity'Tag;
		Entity.Original_Tag	:= APQ.Value( Query, APQ.Column_Index( Query, "original_tag" ) );
		Entity.Filter_Tags	:= APQ.Value( Query, APQ.Column_Index( Query, "filter_tags" ) );

		Property_Lists.Iterate( Info.Properties, Set_Value'Access );
	end Set_Values_From_Query;




	procedure Load( Entity : in out Entity_Type'Class; ID : in ID_Type ) is
		-- load the entity from the database Backend

		package L renames Entity_Information_Lists;
	
		Infos		: L.List;
		Info		: Entity_Information_Type;
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin

			Entity.ID := ID;
			Entity.ID.My_Tag := Entity'Tag;
			-- and now we set the tag of the entity into it's ID

			Will_Load( Entity );
			
			----------------------
			-- SQL Construction --
			----------------------
		
			APQ.Prepare( Query, "SELECT id,original_tag,filter_tags" );
			Append_Column_Names_For_Read( Query, Info, "," );
			APQ.Append( Query, " FROM " & To_String( Info.Table_Name ) );
			APQ.Append( Query, " WHERE id=" );
			ID_Append( Query, Entity.ID.Value );

			-------------------
			-- SQL Execution --
			-------------------
		
			APQ.Execute( Query, Connection );
			APQ.Fetch( Query );
			-- we only reach for the first result as there should be only one
			-- if none is found, No_Tuple is raised. :D
	
			---------------------
			-- Data Processing --
			---------------------

			Set_Values_From_Query( Entity, Query, Connection, Info );

			Was_Loaded( Entity );

			begin
				loop
					-- fetch any remaining data just in case..
					APQ.Fetch( Query );
				end loop;
			exception
				when others => null;
			end;
		end Runner;



		procedure Infos_Iterator( C : in L.Cursor ) is
		begin
			Info := L.Element( C );
			APQ_Provider.Run( My_Provider.all, Runner'Access );
		end Infos_Iterator;


		The_Tag	: Ada.Tags.Tag := Entity'Tag;
	begin
		-- first we find the inheritance stuff
		loop
			Info := Entity_Registry.Get_Information( The_Tag );

			L.Append( Infos, Info );
			The_Tag := Ada.Tags.Parent_Tag( The_Tag );
			exit when The_Tag = Entity_Type'Tag;
		end loop;


		L.Reverse_Iterate( Infos, Infos_Iterator'Access );

	end Load;

	procedure Load( Entity : in out Entity_Type'Class; ID : in Natural ) is
		-- load the entity from the database Backend
		-- it's the same as Load( Entity, To_ID( ID ) );
	begin
		Entity.ID		:= To_ID( ID );
		Entity.ID.My_Tag	:= Entity'Tag;
	
		Load( Entity, Entity.ID );
	end Load;



	procedure Store( Entity : in out Entity_Type'Class ) is
		-- save the entity into the database Backend
		-- if it's a new entity, create a new entry and generates an ID for it (recovering it if needed).
		-- after it has been saved.

	begin
		if Is_New( Entity ) then
			Log( "inserting entity" );
			Will_Insert( Entity );
			Insert( Entity );
			Was_Inserted( Entity );
		else
			Will_Update( Entity );
			Log( "updating entity" );
			Save( Entity );
			Was_Updated( Entity );
		end if;
		
		-- Construct_SQL( Table_Name( Entity'Tag ), Keys, Values );
	end Store;



	function Narrow( Entity : in Entity_Type'Class ) return Entity_Type'Class is
		-- return an entity in it's complete form.
		-- ie, supose the following:
		-- 	type translated_book is new book 
		--
		-- 	store( a_translated_book ) will create a record in the book entity table as well.
		-- 	
		-- 	narrow( a_book ) could return a translated_book entity


	begin
		if Entity.Original_Tag = To_UString( Entity'Tag ) then
			return Entity;
		end if;


		declare
			Result : Entity_Type'Class := Entity_Registry.New_Entity( Entity.Original_Tag );
		begin
			Load( Result, Entity.ID );
		
			return Result;
		end;
	end Narrow;


	procedure Set_Foreign_Key(
				Entity		: in out Entity_Type'Class;
				Related_Entity	: in     Entity_Type'Class
			) is
		-- using the Foreign_Key_property_Type declared in KOW_Ent.Properties,
		-- set the foreign key for the given entity

		Properties : Property_Lists.List := Entity_Registry.Get_Properties(
								Entity_Tag	=> Entity'Tag, 
								Force_all	=> True
							);

		procedure Iterator( C : Property_Lists.Cursor ) is
			Ptr : Entity_Property_Ptr := Property_Lists.Element( C );
		begin
			if	Ptr /= null and then
				Ptr.all in KOW_Ent.Properties.Foreign_Key_Property_Type'Class then

				declare
					use KOW_Ent.Properties;
					P : Foreign_Key_Property_Type'Class :=
						Foreign_Key_property_Type'Class( Ptr.all );
				begin
					if P.Related_Entity_Tag = Related_Entity'Tag then
						P.Setter.all( Entity, Related_Entity.ID );
					end if;
				end;
			end if;
		end Iterator;
	begin
		Property_Lists.Iterate( Properties, Iterator'Access );
	end Set_Foreign_Key;



	function Get_Related_IDs(
				Related_To	: in Entity_Type'Class;
				Entity_Tag	: in String
			) return ID_Array_Type is
		-- get the IDs for all related entities...
		Properties : Property_Lists.List := Entity_Registry.Get_Properties(
								Entity_Tag	=> To_Unbounded_String( Entity_Tag ),
								Force_All	=> True
							);

		Column_Name : Unbounded_String;
		procedure Iterator( C : Property_Lists.Cursor ) is
			Ptr : Entity_property_Ptr := Property_Lists.Element( C );
		begin
			if Ptr /= null then
				if Ptr.all in KOW_ent.Properties.Foreign_key_Property_Type'Class then
					if KOW_ent.Properties.Foreign_key_Property_Type'Class( Ptr.all ).Related_Entity_Tag = Related_To'Tag then
						Column_Name := Ptr.Column_Name;
					end if;
				end if;
			end if;
		end iterator;




		My_Ids : Id_Array_Ptr;

		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
			Info	: Entity_Information_Type := Entity_Registry.Get_Information( To_Unbounded_String( Entity_Tag ) );


			function Inner_Get_All_Ids return ID_Array_Type is
				Ret : ID_Array_Type( 1 .. 1 );
				TMP_Id : Integer;
				ID : ID_Type;
			
			begin
				APQ.Fetch( Query );
	
				TMP_Id := APQ.Value( Query, APQ.Column_Index( Query, "id" ) );
				ID := To_ID( TMP_Id );
				Ret( 1 ) := ID;
	
				return Ret & Inner_Get_All_Ids;
			exception
				when APQ.No_Tuple => return Ret( 1 .. 0 );
			end Inner_Get_All_Ids;
		begin
			APQ.Prepare( Query, "SELECT id FROM " & To_String( Info.Table_Name ) );
			APQ.Append( Query, " WHERE " & To_String( Column_Name ) );
			APQ.Append( Query, "=" & To_String( Related_To.ID ) );
			APQ.Execute( Query, Connection );

			My_Ids := new Id_Array_Type'( Inner_Get_All_Ids );
		end Runner;
	begin

		Property_Lists.Iterate( Properties, Iterator'Access );

		if Column_Name = Null_Unbounded_String then
			raise CONSTRAINT_ERROR with "can't find related entity";
		end if;

		APQ_Provider.Run( My_Provider.all, Runner'Access );

		declare
			My_Inner_Ids : Id_Array_Type := My_Ids.all;
		begin
			Free( My_ids );
			return My_Inner_ids;
		end;
	exception
		when APQ.No_Tuple => 
		declare
			Ret : ID_Array_Type( 1 .. 0 );
		begin
			return Ret;
		end;

	end Get_Related_IDs;





	-- NOTE :: How the Entity Labels should work ::
	--
	-- They should be stored in an instance of KOW_Config·Generic_Registry
	-- Each configuration file name as in name1.name2 is mapped to the tag name1.name2 (all lower case).
	-- The entity label is set by the key "entity_label". All other labels are the property name. All lower case.


	function Standard_Label_Getter_Factory( Id : in String; Config : KOW_Config.Config_File ) return Label_Getter is
		G: Label_Getter;
	begin
		G.Id		:= To_Unbounded_String( Id );
		G.Config	:= Config;
		return G;
	end Standard_Label_Getter_Factory;


	function Get_Label( Getter : in Label_Getter; Locale : in KOW_Lib.Locales.Locale ) return Unbounded_String is
		-- get a label for this getter
	begin
		return KOW_Config.Element( Getter.Config, To_Unbounded_String( "entity_label" ), Locale.CODE );
	end Get_Label;

	function Get_Label( Getter : in Label_Getter; Property : in Unbounded_String; Locale : in KOW_Lib.Locales.Locale ) return Unbounded_String is
		-- get a label for a property in this getter
	begin
		return KOW_Config.Element( Getter.Config, Property, Locale.CODE );
	end Get_Label;

	function Get_Label(
				Entity : in Entity_Type'Class;
				Locale : in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return String is
		-- get the Label for this entity type as string
	begin
		return To_String( Get_Label( Entity, Locale ) );
	end Get_Label;
	


	function Get_Label(
				Entity : in Entity_Type'Class;
				Locale : in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String is
		Str_Tag  : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity'Tag )
					);
	begin
		KOW_Lib.String_Util.Str_Replace( From => '.', To => '/', Str => Str_Tag );
		return Get_Label(
				Labels.Registry.Get( '/' & Str_Tag ),
				Locale
			);
	exception
		when others =>
			return To_Unbounded_String( '[' & Ada.Tags.Expanded_Name( Entity'Tag ) & ']' );
	end Get_Label;
	
	function Get_Label(
				Entity_Tag	: in Ada.Tags.Tag;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String is
		-- get the Label for this entity type as unbounded_string
		Str_Tag  : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity_Tag )
					);
	begin
		KOW_Lib.String_Util.Str_Replace( From => '.', To => '/', Str => Str_Tag );
		return Get_Label(
				Labels.Registry.Get( '/' & Str_Tag ),
				Locale
			);
	end Get_Label;


	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return String is
		-- get the Label for the given property of this entity type as string
	begin
		return To_String( Get_Label( Entity, To_Unbounded_String( Property ), Locale ) );
	end Get_Label;
	

	
	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String is
		-- get the Label for given property of this entity type as unbounded_string
	begin
		return Get_Label( Entity, To_Unbounded_String( Property ), Locale );
	end Get_Label;



	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in Unbounded_String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return String is
		-- get the Label for the given property of this entity type as string
	begin
		return To_String( Get_Label( Entity, Property, Locale ) );
	end Get_Label;


	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in Unbounded_String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String is
		-- get the Label for given property of this entity type as unbounded_string
		Str_Tag  : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity'Tag )
					);
	begin
		KOW_Lib.String_Util.Str_Replace( From => '.', To => '/', Str => Str_Tag );
		return Get_Label(
				Labels.Registry.Get( '/' & Str_Tag ),
				Property,
				Locale
			);
	exception
		when others =>
			return To_Unbounded_String( '[' & Ada.Tags.Expanded_Name( Entity'Tag ) & "::" )  & Property & ']';
	end Get_Label;


	function Get_Label(
				Entity_tag	: in Ada.Tags.Tag;
				Property	: in String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String is
		Str_Tag  : String := Ada.Characters.Handling.To_Lower(
						Ada.Tags.Expanded_Name( Entity_Tag )
					);
	begin
		KOW_Lib.String_Util.Str_Replace( From => '.', To => '/', Str => Str_Tag );
		return Get_Label(
				Labels.Registry.Get( '/' & Str_Tag ),
				To_Unbounded_String( Property ),
				Locale
			);
	end Get_Label;




	function Hash(Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type is
	begin
		return Ada.Strings.Hash (Ada.Strings.Unbounded.To_String( Key ) );
	end Hash;


	protected body Metadata_Registry is
		-- take care of allocating slots for given metadata types...

		procedure Allocate( Index : out Positive ) is
			-- allocate the metadata slot returning it's index..
		begin
			if Next > Entity_Property_Metadata_Array'Last then
				raise Constraint_Error with "no more property metadata slots left...";
			end if;
			Index := Next;
			Next := Next + 1;
		end Allocate;
	end Metadata_Registry;


	function To_Json_Data(
				Property	: in     Entity_Property_Type;
				Entity		: in     Entity_Type'Class
			) return KOW_Lib.Json.Json_Data_Type is
		-- get the property as a json data type
	begin
		return KOW_Lib.Json.To_Data(
					Get_Property( Entity_Property_Type'Class( Property ), Entity )
				);
	end To_Json_Data;


	function Should_Read( Property : in Entity_Property_Type ) return Boolean is
		-- Asks if the value should be set from the database or not
		-- Default :: true
		-- This is here for the Password_Property_Type (that doesn't read from the database)
	begin
		return True;
	end Should_Read;

	function Should_Store( Property : in Entity_Property_Type; Entity : in Entity_Type'Class ) return Boolean is
		-- asks if the property for the given entity should be stored or no
		-- useful to track if the user password has been changed and need to be stored back or not
		-- Default :: true
	begin
		return True;
	end Should_Store;


	----------------------------
	-- SQL Creation Framework --
	----------------------------


	--
	-- Entity kind related methods...
	--

	procedure Prepare_Create_For_Entity(
				Query	: in out APQ.Root_Query_Type'Class;
				Tag	: in     Ada.Tags.Tag
			) is
		-- prepare the query for the given entity
	begin
		Prepare_Create_For_Entity(
				Query	=> Query,
				Tag	=> To_UString( Tag )
			);
	end prepare_Create_For_Entity;



	procedure Prepare_Create_For_Entity(
				Query	: in out APQ.Root_Query_Type'Class;
				Tag	: in     Unbounded_String
			) is
		-- prepare the query for the given entity
		use APQ;


		Info : Entity_Information_Type := Entity_Registry.Get_Information( Tag );



		procedure Iterator( C : Property_Lists.Cursor ) is
		begin
			Append_Create_Table(
					Property	=> Property_lists.Element( C ).all,
					Query		=> Query
				);
			Append( Query, "," );
		end Iterator;


	


		procedure Unique_Keys_Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
			CN : String := To_String( KOW_Lib.UString_Vectors.Element( C ) );
		begin
		
			Append(
					Query,
					", UNIQUE KEY `" & CN & "` (`" & CN & "`)"
				);
		end Unique_Keys_Iterator;


	begin
		Prepare(
				Query,
				"CREATE TABLE " & To_String( Info.Table_Name ) & "("
			);

		if Ada.Tags.Parent_Tag( Info.Entity_Tag ) = Entity_Type'Tag AND Info.Id_Generator = null then
			Append(
					Query,
					"`id` int(11) NOT NULL auto_increment,"
				);
		else
			Append(
					Query,
					"`id` int(11) NOT NULL,"
				);
		end if;


		Append(
				Query,
				"`original_tag` varchar(255) NOT NULL,"
			);


		Append(
				Query,
				"`filter_tags` varchar(255) NOT NULL,"
			);



		Property_Lists.Iterate( Info.Properties, Iterator'Access );

		Append(
				Query,
				"PRIMARY KEY  (`id`)"
			);

		
		KOW_Lib.UString_Vectors.Iterate( Info.Unique_Keys, Unique_Keys_Iterator'Access );


		Append(
				Query,
				") ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;"
			);
		
	end Prepare_Create_For_Entity;
	


	function Get_Create_For_Entity( Tag : in Ada.Tags.Tag ) return String is
		-- get the SQL string for creating the given entity using APQ Provider
	begin
		return Get_Create_For_Entity( To_UString( Tag ) );
	end Get_Create_For_Entity;


	
	function Get_Create_For_Entity( Tag : in Unbounded_String ) return String is
	-- get the SQL string for creating the given entity using APQ Provider
		Buffer : Unbounded_String;

		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			-- the only thing we do here is to create the query and
			-- then run the SQL creation procedure for the given entity
			-- storing it in the buffer..
			--
			--
			-- then we return it.

			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin

			Prepare_Create_For_Entity(
					Query,
					Tag
				);
			Buffer := To_Unbounded_String( APQ.To_String( Query ) );
		end Runner;
			
		
	begin
		APQ_Provider.Run( My_Provider.all, Runner'Access );


		return To_String( Buffer );
	end Get_Create_For_Entity;



	procedure Run_Create_For_Entity( Tag : in Ada.Tags.Tag ) is
		-- create and run the query for the given entity using APQ Provider
	begin
		Run_Create_For_Entity( To_UString( Tag ) );
	end Run_Create_For_Entity;


	procedure Run_Create_For_Entity( Tag : in Unbounded_String ) is
		-- create and run the query for the given entity using APQ Provider

		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			-- the only thing we do here is to create the query and
			-- then run the SQL creation procedure for the given entity
			-- storing it in the buffer..
			--
			--
			-- then we return it.

			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin

			Prepare_Create_For_Entity(
					Query,
					Tag
				);

			Run_And_Fetch(
					Query,
					Connection
				);
		end Runner;
	begin
		APQ_Provider.Run( My_Provider.all, Runner'Access );
	end Run_Create_For_Entity;


	--
	-- global methods
	--

	function Get_Create( Append_Dump_if_Exists : Boolean := False ) return String is
		-- get the table creation SQL for every entity in the entity registry using APQ Provider
		

		All_Entities : Entity_Information_Maps.Map := Entity_Registry.Get_Informations_Map;

		Buffer : Unbounded_String;

		Line_Break : constant Character := Ada.Characters.Latin_1.LF;


		procedure Iterator( C : in Entity_Information_Maps.Cursor ) is
			Key	: Unbounded_String := Entity_Information_Maps.Key( C );
			Element : Entity_Information_Type := Entity_Information_Maps.Element( C );
		begin
			if Append_Dump_If_Exists then
				Append(
						Buffer,
						"DROP TABLE IF EXISTS "
					);
				Append( Buffer, Element.Table_Name );
				Append( Buffer, ";" & Line_Break );
			end if;

			Append( Buffer, Get_Create_For_Entity( Key ) & Line_Break );
			-- TODO :: implement a get_Create_for_entity using the entity information type...

		end Iterator;
			
	begin

		Buffer := To_UString(
				"-- This code has been generated automatically by the KOW Ent framework." &
				Line_Break &
				Line_Break &
				Line_Break
			);

		Entity_Information_Maps.Iterate(
				All_Entities,
				Iterator'Access
			);


		return To_String( Buffer );

	end Get_Create;


	procedure Run_Create( Dump_If_Exists : Boolean := False ) is
		-- create the entire DB structure using APQ Provider
		All_Entities : Entity_Information_Maps.Map := Entity_Registry.Get_Informations_Map;

		procedure Iterator( C : in Entity_Information_Maps.Cursor ) is
			Key	: Unbounded_String := Entity_Information_Maps.Key( C );
			Element : Entity_Information_Type := Entity_Information_Maps.Element( C );




			procedure Dump_Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
				Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
			begin
				APQ.Prepare(
						Query,
						"DROP TABLE IF EXISTS " &
							To_String( Element.Table_Name )
					);

				Run_And_Fetch(
						Query,
						Connection
					);
			end Dump_Runner;

		begin
			if Dump_If_Exists then
				APQ_Provider.Run( My_Provider.all, Dump_Runner'Access );
			end if;

			Run_Create_For_Entity( Key );
		end Iterator;
			
	begin
		Entity_Information_Maps.Iterate(
				All_Entities,
				Iterator'Access
			);
	end Run_Create;






	-------------------------
	-- Entity Registration --
	-------------------------




	protected body Entity_Registry is
		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Table_Name	: in String;
					Id_Generator	: in Id_Generator_Type := Null;
					Factory		: Factory_Type ) is
			-- register an Entity into the KOW_Ent engine
			-- Table_Name is the table name to be used.

			Info	: Entity_Information_Type;
		begin
			Info.Entity_Tag   := Entity_Tag;
			Info.Table_Name	  := To_Unbounded_String( Table_Name );
			Info.Id_Generator := Id_Generator;
			Info.Factory      := Factory;

			Entity_Information_Maps.Insert(
				Container	=> My_Entities,
				Key		=> To_UString( Entity_Tag ),
				New_Item	=> Info
			);
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Dupĺicated entity :: """ & Expanded_Name( Entity_Tag ) & """";

		end Register;
	
		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Id_Generator	: in Id_Generator_Type := Null;
					Factory		: Factory_Type ) is
			-- register an Entity into the KOW_Ent engine
			-- Auto generate the table name (using the Tag)
		begin
			Register(
				Entity_Tag	=> Entity_Tag,
				Table_Name	=> Expanded_Name( Entity_Tag ),
				Id_Generator	=> Id_Generator,
				Factory		=> Factory
			);
		end Register;
	
	
		procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
					Property	: in Entity_Property_Ptr;
					Is_Unique	: in Boolean := False) is
			-- add another property to this entity
			Info : Entity_Information_Type;
		begin
			Info := Entity_Information_Maps.Element( My_Entities, To_UString( Entity_Tag ) );
			Property_Lists.Append( Info.Properties, Property );
			-- we add the entity to the list we retrieved.

			if Is_Unique then
				KOW_Lib.UString_Vectors.Append(
							Info.Unique_Keys,
							Property.Column_Name
						);
			end if;
			
			Entity_Information_Maps.Include( My_Entities, To_UString( Entity_Tag ), Info );
			-- and now we replace the existing entity registry
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ & Expanded_Name( Entity_Tag ) & """";
		end Add_Property;


		procedure Replace_Property(
					Entity_Tag	: in Ada.Tags.Tag;
					Property	: in Entity_Property_Ptr
				) is
			-- replace an existing property by the column_name and maintaining the unicity parameter

			Property_Position : Property_Lists.Cursor;
			Info : Entity_Information_Type;


			procedure Iterator( C : in Property_Lists.Cursor ) is
			begin
				if Property_Lists.Element( C ).Column_Name = Property.Column_Name then
					Property_Position := C;
				end if;
			end Iterator;
		begin
			Info := Entity_Information_Maps.Element( My_Entities, To_UString( Entity_Tag ) );

			Property_lists.Iterate( Info.Properties, Iterator'Access );
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Trying to replace an unknown property :: """ &
								To_String( Property.Column_Name ) & " of entity " &
								Expanded_name( Entity_Tag );
		end Replace_Property;


		function Get_Information( Entity_Tag : in Ada.Tags.Tag ) return Entity_Information_Type is
		begin
			return Entity_Information_Maps.Element( My_Entities, To_UString( Entity_Tag ) );
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ & Expanded_Name( Entity_Tag ) & """";
		end Get_Information;

		function Get_Information( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Entity_Information_Type is
			-- retrieve the entity information by it's tag's expanded name
		begin
			return Entity_Information_Maps.Element( My_Entities, To_UString( Entity_Tag ) );
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ &  To_String ( Entity_Tag ) & """";
		end Get_Information;

		function Get_Properties( Entity_Tag : in Ada.Tags.Tag; Force_All : Boolean := False ) return Property_Lists.List is
			-- retrieve the property list for the given entity;

			Info		: Entity_Information_Type := Get_Information( Entity_Tag );
			The_Tag		: Ada.Tags.Tag := Ada.Tags.Parent_Tag( Info.Entity_Tag );
			Properties	: Property_Lists.List := Info.Properties;
		begin

			if Force_All then
				loop
					exit when The_Tag = Entity_Type'Tag;
					Info := Get_Information( The_Tag );
					Prepend_All( Properties, Info.Properties );
					The_Tag := Ada.Tags.Parent_Tag( The_Tag );
				end loop;
			end if;


			return Properties;

		end Get_Properties;
		
		function Get_Properties( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String; Force_All : Boolean := False ) return Property_Lists.List is
			-- retrieve the property list for the given entity;

			Info		: Entity_Information_Type := Get_Information( Entity_Tag );
			The_Tag		: Ada.Tags.Tag := Ada.Tags.Parent_Tag( Info.Entity_Tag );
			Properties	: Property_Lists.List := Info.Properties;
		begin
			if Force_All then
				loop
					exit when The_Tag = Entity_Type'Tag;
					Info := Get_Information( The_Tag );
					Prepend_All( Properties, Info.Properties );
					The_Tag := Ada.Tags.Parent_Tag( The_Tag );
				end loop;
			end if;


			return Properties;

		end Get_Properties;


		function New_Entity( Entity_Tag : in Ada.Tags.Tag ) return Entity_Type'Class is
			-- produce a new entity
			Info : Entity_information_Type := Get_Information( Entity_Tag );
		begin
			if Info.Factory = Null then
				raise No_Factory with "No factory registered for entity :: """ & Expanded_Name( Entity_Tag ) & """";
			end if;

			return Info.Factory.all;
		end New_Entity;


		function New_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Entity_Type'Class is
			-- produce a new entity
			Info : Entity_information_Type := Get_Information( Entity_Tag );
		begin
			if Info.Factory = Null then
				raise No_Factory with "No factory registered for entity :: """ & Ada.Strings.Unbounded.To_String( Entity_Tag ) & """";
			end if;

			return Info.Factory.all;
		end New_Entity;



		function Get_Informations_Map return Entity_Information_Maps.Map is
		begin
			return My_Entities;
		end Get_Informations_Map;

	end Entity_Registry;


	function Load_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String; ID : in Id_Type ) return Entity_Type'Class is
		Ent : Entity_Type'Class := New_Entity( Entity_Tag );
	begin
		Load( Ent, ID );
		return Ent;
	end Load_Entity;

	function Load_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String; ID : in Natural ) return Entity_Type'Class is
	begin
		return Load_Entity( Entity_Tag, To_ID( ID ) );
	end Load_Entity;

	function Load_Entity( Entity_Tag : in Ada.Tags.Tag; ID : in Id_Type ) return Entity_Type'Class is
		Ent : Entity_Type'Class := New_Entity( Entity_Tag );
	begin
		Load( Ent, ID );
		return Ent;
	end Load_Entity;

	function Load_Entity( Entity_Tag : in Ada.Tags.Tag; ID : in Natural ) return Entity_Type'Class is
	begin
		return Load_Entity( Entity_Tag, To_ID( ID ) );
	end Load_Entity;



	function Get_Property( Entity_Tag : in Ada.Tags.Tag; Column_Name : in String; Force_All : Boolean := False ) return Entity_Property_Ptr is
		Property : Entity_Property_Ptr;

		procedure Iterator( C : Property_Lists.Cursor ) is
			Current_Property : Entity_Property_Ptr := Property_Lists.Element( C );
		begin
			if Current_Property.Column_Name = Column_Name then
				Property := Current_Property;
			end if;
		end Iterator;
	begin
		Property_Lists.Iterate( Get_Properties( Entity_Tag, Force_All ), Iterator'Access );

		return Property;
	end Get_Property;

-- private ::

	----------------------------------------------
	-- Auxiliar Functions for Entity Management --
	----------------------------------------------

	procedure Save( Entity : in out Entity_Type'Class ) is
		-- save the existing entity into the database Backend
	
		package L renames Entity_Information_Lists;

		The_Tag		: Ada.Tags.Tag := Entity'Tag;
		Info		: Entity_Information_Type;
		Infos		: L.List;



		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );


			procedure Update_Appender( C : Property_Lists.Cursor ) is
				Property: Entity_Property_Ptr := Property_Lists.Element( C );
			begin
				if Should_Store( Property.all, Entity ) and not Property.Immutable then
	
					if Property /= null then
						APQ.Append(
							Query,
							"," & To_String( Property.all.Column_Name ) & "="
						);
					
	
						Get_Property( Property.all, Entity, Query, Connection );
					else
						raise Constraint_Error with "Some null property exists in """& Expanded_Name( Entity'Tag ) & """";
					end if;
				end if;
			end Update_Appender;
				
	
		begin
			APQ.Prepare(
				Query,
				"UPDATE " &
					To_String( Info.Table_Name ) &
					" SET filter_tags="
			);

			APQ.Append_Quoted( Query, Connection, Entity.Filter_Tags );
				
			Property_Lists.Iterate( Info.Properties, Update_Appender'Access );


			APQ.Append(
				Query,
				" WHERE id="
			);
	
			ID_Append(
				Query,
				Entity.ID.Value
			);

			APQ.Execute(
					Query,
					Connection
				);
		end Runner;




		procedure Infos_Iterator( C : in L.Cursor ) is
		begin
			Info := L.Element( C );
			APQ_Provider.Run( My_Provider.all, Runner'Access );
		end Infos_Iterator;


	begin
		-- first we find the inheritance stuff
		loop
			Info := Entity_Registry.Get_Information( The_Tag );

			if Info.Id_Generator /= null then
				-- check if the user didn't temper with the ID
				declare
					use APQ;
					TMP_Id : Id_Type := Info.Id_Generator.All( Entity );
				begin
					if TMP_Id.Value /= Entity.Id.Value then
						raise CONSTRAINT_ERROR with "trying to temper with code generated entity ID";
					end if;
				end;
			end if;


			L.Append( Infos, Info );
			The_Tag := Ada.Tags.Parent_Tag( The_Tag );
			exit when The_Tag = Entity_Type'Tag;
		end loop;

		L.Reverse_Iterate( Infos, Infos_Iterator'Access );

	end Save;
	

	procedure Insert( Entity : in out Entity_Type'Class ) is
		-- save the entity into the database Backend
		-- if it's a new entity, create a new entry and generates an ID for it.
		-- after it has been saved
		

		package L renames Entity_Information_Lists;

		The_Tag		: Ada.Tags.Tag := Entity'Tag;
		Info		: Entity_Information_Type;
		Infos		: L.List;

		Recover_ID	: Boolean	:= True;


		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			ID	: Id_Type;
		
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
	
			First_Element : Boolean := True;

			procedure Insert_Appender( C: in Property_Lists.Cursor ) is
				Property: Entity_Property_Ptr := Property_Lists.Element( C );
			begin
				if not Should_Store( Property.all, Entity ) then
					-- if there is nothing to store here, simply ignores this one property
					return;
				end if;
	
				if not First_Element then
					APQ.Append( Query, "," );
				else
					First_Element := False;
				end if;
				if Property /= null then
					Get_Property( Property.all, Entity, Query, Connection );
				else
					raise Constraint_Error with "Some property is null at """ & Expanded_Name( Entity'Tag ) & """";
				end if;
			end Insert_Appender;

		begin
			
			---------------
			-- SQL Setup --
			---------------
			
			APQ.Prepare(
				Query,
				"INSERT INTO " & To_String( Info.Table_Name ) & "("
			);


			if Info.Id_Generator /= NULL or Ada.Tags.Parent_Tag( Info.Entity_Tag ) /= Entity_Type'Tag then
				Append_Column_Names_For_Store( Query, Info.Properties, Entity, "," );
				APQ.Append(
					Query,
					"id,original_tag,filter_tags"
				);
			else
				Append_Column_Names_For_Store( Query, Info.Properties, Entity, "," );
				APQ.Append(
					Query,
					"original_tag,filter_tags"
				);
			end if;
	
			APQ.Append( Query, ") VALUES(" );

			Property_Lists.Iterate( Info.Properties, Insert_Appender'Access );


			if Info.Id_Generator /= NULL then
				ID := Info.Id_Generator.all( Entity );
				if not First_Element then
					APQ.Append(
						Query,
						","
					);
				end if;
				ID_Append(
					Query,
					ID.Value
				);
			elsif Ada.Tags.Parent_Tag( Info.Entity_Tag ) /= Entity_Type'Tag then
				if not First_Element then
					APQ.Append(
						Query,
						","
					);
				else
					First_Element := False;
				end if;
				ID_Append(
					Query,
					Entity.ID.Value
				);
			end if;

			if not First_Element then
				APQ.Append(
					Query,
					","
				);
			end if;
			APQ.Append_Quoted(
				Query,
				Connection,
				Ada.Tags.Expanded_Name( Entity'Tag )
			);
			APQ.Append(
				Query,
				","
			);
			APQ.Append_Quoted(
				Query,
				Connection,
				Entity.Filter_Tags
			);
			Entity.Original_Tag := To_Unbounded_String( Ada.Tags.Expanded_Name( Entity'Tag ) );

			APQ.Append( Query, ")" );

			-------------------
			-- SQL Execution --
			-------------------

			Log( "Will run :: " & APQ.To_String( Query ) , KOW_Lib.Log.Level_Debug );
			APQ.Execute( Query, Connection );

			if Recover_ID then
				if Info.Id_Generator /= NULL then
					Entity.ID := ID;
					Entity.ID.My_Tag := Entity'Tag;
				else
					declare
						OID : APQ.Row_Id_Type;
					begin
						OID := APQ.Command_OID( Query );
						Entity.ID := To_Id( Natural( OID ) );
						Entity.ID.My_Tag := Entity'Tag;
						Log( "Recovered ID :: " & KOW_Ent.To_String( Entity.ID ) );
					end;
				end if;
				Recover_ID := False;
			end if;
		end Runner;

		procedure Infos_Iterator( C : in L.Cursor ) is
		begin
			Info := L.Element( C );
			APQ_Provider.Run( My_Provider.all, Runner'Access );
		end Infos_Iterator;


	begin
		-- first we find the inheritance stuff
		loop
			Info := Entity_Registry.Get_Information( The_Tag );

			L.Append( Infos, Info );
			The_Tag := Ada.Tags.Parent_Tag( The_Tag );
			exit when The_Tag = Entity_Type'Tag;
		end loop;

		L.Reverse_Iterate( Infos, Infos_Iterator'Access );
	end Insert;


	------------------------------
	-- Other Auxiliar Functions --
	------------------------------

	procedure Append_Column_Names_For_Read(
				Query	: in out APQ.Root_Query_Type'Class;
				Info	: in     Entity_Information_Type;
				Before	: in     String := ""
			) is
		-- this procedure is used internally to set a column of values in the fashion of:
		-- a,b,c,d
		-- where a, b, c and d are columns of this entity
		-- implementation is at the end of the file

		First_Property	: Boolean := True;
		Table_Name	: constant String := To_String( Info.Table_Name ) & '.';

		procedure Set_Column_Names( C: Property_Lists.Cursor ) is
			use Property_Lists;
			Column: String := To_String( Element( C ).all.Column_Name );
		begin
			if Should_Read( Element( C ).all ) then
				if not First_Property then
					APQ.Append( Query, "," );
				else
					APQ.Append( Query, Before );
					First_Property := False;
				end if;

				APQ.Append( Query, Table_Name & Column & " as " & Column );
			end if;
		end Set_Column_Names;

	begin
		Property_Lists.Iterate( Info.Properties, Set_Column_Names'Access );
	end Append_Column_Names_For_Read;


	procedure Append_Column_Names_For_Store( Query : in out APQ.Root_Query_Type'Class; Properties: Property_Lists.List; Entity : in Entity_Type'Class; After: in String := "" ) is
		-- this procedure is used internally to set a column of values in the fashion of:
		-- a,b,c,d
		-- where a, b, c and d are columns of this entity
		-- implementation is at the end of the file
		--
		-- append "after" only if has elements

		First_Property: Boolean := True;

		procedure Set_Column_Names( C: Property_Lists.Cursor ) is
			use Property_Lists;
			Column: String := To_String( Element( C ).all.Column_Name );
		begin
			if Should_Store( Element( C ).all, Entity ) then
				if not First_Property then
					APQ.Append( Query, "," );
				else
					First_Property := False;
				end if;

				APQ.Append( Query, Column );
			end if;
		end Set_Column_Names;

	begin
		Property_Lists.Iterate( Properties, Set_Column_Names'Access );

		if not First_Property then
			APQ.Append( Query, After );
		end if;
	end Append_Column_Names_For_Store;


begin
	Labels.Factory_Registry.Register( "standard", Standard_Label_Getter_Factory'Access );
end KOW_Ent;