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
-- Main KOW_Ent package.                                                    --
--                                                                          --
-- KOW_Ent is reponsible for handling persistent data in your application   --
-- stored in Database backends using the native DB types.                   --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with APQ;
with APQ_Provider;
with KOW_Ent.Data_Storages;

package KOW_Ent.DB is



	---------------------
	-- Every day Setup --
	---------------------

	procedure Setup;
	-- load the kow_ent.cfg APQ_Provider file and setup the provider


	-------------------------------------
	-- Installation of the Core Schema --
	-------------------------------------


	-- As each database vendor choses a different way for informing what tables are available
	-- with no universal API KOW_Ent will store a redundant record of such information.
	--
	-- This record links entity alias to entity tag and also the version.

	ALREADY_INSTALLED : Exception;
	-- throws whenever the instalce is already installed

	procedure Install_Table_Schema;
	-- this will do several things:
	-- 	1. test the connection
	-- 	2. check if the table is installed, if so raises ALREADY_INSTALLED
	-- 	3. create the core schema


	procedure Insert_Table_Schema( Template_Entity : in out KOW_Ent.Entity_Type'Class );
	-- this will insert some valuable information about the given entity into the
	-- schema

private

	Schema_Table_Name : constant String := "KOWENT_SCHEMA";

	package Column_Names is
		Entity_Alias	: constant String := "ENTITY_ALIAS";
		Entity_Tag	: constant String := "ENTITY_TAG";
		Schema_Version	: constant String := "SCHEMA_VERSION";
		Storage_Type	: constant String := "STORAGE_TYPE";
		Storage_Version	: constant String := "STORAGE_VERSION";
	end Column_Names;



	Provider : APQ_Provider.Connection_Provider_Ptr;
	-- all the data storages instances in instances of the
	-- generic child packages will actually perform queries in this provider


end KOW_Ent.DB;
