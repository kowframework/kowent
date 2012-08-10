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

------------------------------------------------------------------------------
-- Main KOW_Ent package.                                                    --
--                                                                          --
-- KOW_Ent is reponsible for handling persistent data in your application   --
-- stored in Database backends using the native DB types.                   --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with APQ;
with APQ_Provider;
with KOW_Config;
with KOW_Ent.Data_Storages;



package body KOW_Ent.DB is

	---------------------
	-- Every Day Setup --
	---------------------

	procedure Setup is
		-- looks for the kow_ent configuration file and initializes the provider
	begin
		Provider := new APQ_Provider.Connection_Provider_Type;
		Provider.Setup( KOW_Config.New_Config_File( "kow_ent" ) );
	end Setup;



	-------------------------------------
	-- Installation of the Core Schema --
	-------------------------------------

	Is_First_Run : Boolean := True;
	procedure Install_Table_Schema is
		-- this will do several things:
		-- 	1. test the connection
		-- 	2. check if the table is installed, if so raises ALREADY_INSTALLED
		-- 	3. create the core schema
		procedure Runner ( C : in out APQ.Root_Connection_Type'Class ) is
			use APQ;
			use Column_Names;
			Q : Root_Query_Type'Class := New_Query( C );
		begin
			-- test the connection:
			Prepare( Q, "SELECT 1" );
			Execute( Q, C );
			Fetch( Q );

			Clear( Q );

			begin
				Prepare( Q, "SELECT * from " & Schema_Table_Name );
				Execute( Q, C );
				loop
					Fetch( Q );
				end loop;
			exception
				when APQ.No_Tuple =>
					raise ALREADY_INSTALLED with "please remove manually - this will erase all your data";
				when APQ.SQL_Error =>
					-- it means the DB doesn't exist
					null;
			end;

			Clear( Q );


			Prepare( Q, "CREATE TABLE " & Schema_Table_Name & "(" );

			Append( Q, Entity_Alias		& " VARCHAR(" & Integer'Image( Entity_Alias_Type'Length ) & ") NOT NULL," );
			Append( Q, Entity_Tag		& " VARCHAR(250) NOT NULL," );
			Append( Q, Schema_Version	& " VARCHAR(15) NOT NULL," );
			Append( Q, Storage_Type		& " VARCHAR(50) NOT NULL," );
			Append( Q, Storage_Version	& " VARCHAR(15) NOT NULL," );
			Append( Q, "PRIMARY KEY(" & Entity_Alias & ")" );

			Append( Q, ")" );

			Execute( Q, C );
		end Runner;
	begin
		if Is_First_Run then
			Is_First_Run := False;
		else
			return;
		end if;
		APQ_Provider.Run(
					Provider		=> KOW_Ent.DB.Provider.all,
					Connection_Runner	=> Runner'Access,
					Queue_On_OOI		=> False
				);
	end Install_Table_Schema;

	procedure Insert_Table_Schema( Template_Entity : in out KOW_Ent.Entity_Type'Class ) is
		-- this will insert some valuable information about the given entity into the
		-- schema

		procedure Runner(  C : in out APQ.Root_Connection_Type'Class ) is
			use APQ;
			use Column_Names;
			Q	: Root_Query_Type'Class := New_Query( C );
			Storage	: constant Data_Storage_Ptr := KOW_Ent.Data_Storages.Get_Data_Storage( Template_Entity'Tag );

			procedure A( Str : in String; Put_Comma : Boolean; Quoted : Boolean := False ) is
			begin
				if Quoted then
					Append_Quoted( Q, C, Str );
				else
					Append( Q, Str );
				end if;

				if Put_Comma then
					Append( Q, "," );
				end if;
			end A;
		begin
			A( "INSERT INTO " & Schema_Table_Name & "(", False );
			A( Entity_Alias		,  True );
			A( Entity_Tag		,  True );
			A( Schema_Version	,  True );
			A( Storage_Type		,  True );
			A( Storage_Version	, False );

			A( ") VALUES (", False );
			A( Trim( Get_Alias( Template_Entity ) )			,  True, True );
			A( Ada.Tags.Expanded_Name( Template_Entity'Tag )	,  True, True );
			A( Version_Of( Template_Entity )		 	,  True, True );
			A( Type_of( Storage.all )				,  True, True );
			A( Version_Of( Storage.all )				, False, True );

			A( ")", False );

			Execute( Q, C );
		end Runner;
	begin
		APQ_Provider.Run(
					Provider		=> KOW_Ent.DB.Provider.all,
					Connection_Runner	=> Runner'Access,
					Queue_on_OOI		=> False
				);
	end Insert_Table_Schema;
end KOW_Ent.DB;
