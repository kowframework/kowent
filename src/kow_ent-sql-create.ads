------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
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
-- Package for creating SQL queries for table creation in KOW Ent           --
------------------------------------------------------------------------------




--------------
-- Ada 2005 --
--------------
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;				use KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;


package KOW_Ent.SQL.Create is

	type Create_Generator_Type is tagged private;
	-- all the calls in this type are dynamic dispatched


	procedure Generate_Create(
			Generator	: in out Create_Generator_Type;
			Template_Entity	: in out KOW_Ent.Entity_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);
	-- generate the create query


	--------------------------
	-- Column Specification --
	--------------------------
	
	procedure Append_Column_Specification(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Ptr;
			Q		: in out APQ.Root_Query_Type'Class
		);
	-- append a line with the column specification (no specification separator is set)


	----------------------
	-- ID Specification --
	----------------------

	procedure Append_Id_Specification(
			Generator	: in out Create_Generator_Type;
			Q		: in out APQ.Root_Query_Type'Class
		);
	-- append the index id specification




	-----------------------------------------
	-- Column Specification Helper Methods --
	-----------------------------------------

	-- numeric values
	procedure Spec_Smallint(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Integer(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Bigint(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Real(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Double(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Serial(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Bigserial(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);



	-- date/time/timestamp

	procedure Spec_Date(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Time(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Timestamp(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);


	-- hour/minute/second

	procedure Spec_Hour(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Minute(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Second(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);



	-- string

	procedure Spec_String(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);



	---------------------------
	-- Auxiliar Spec Methods --
	---------------------------


	procedure General_Spec(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Type_Spec	: in     String;
			Q		: in out APQ.Root_Query_Type'Class
		);

	procedure Spec_Auto_Increment(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);
	-- insert the auto-increment notation for ct_lib and mysql
	-- for postgresql just check if the value is of serial type (it's enough in this vendor).
	--
	-- if not of serial type, raises program_error with informative message
	


	--------------------------
	-- Index Generator Type --
	--------------------------

--	type Index_Generator_Type is tagged private;

private
	type Create_Generator_Type is tagged record
		Id_Property : KOW_Ent.Property_Ptr;
		-- this is set by append_column_information
		-- 
	end record;


end KOW_Ent.SQL.Create;


