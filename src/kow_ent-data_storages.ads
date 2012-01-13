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
-- Data Storage definition                                                  --
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

package KOW_Ent.Data_Storages is


	---------------------------
	-- The Data Storage Type --
	---------------------------

	type Data_Storage_Type is abstract new Data_Storage_Interface with null record;
	-- only extending the API for the Data Storage in an organized way



	--------------
	-- Registry --
	--------------

	function Get_Alias(
				Data_Entity	: in    Data_Storage_Type;
				Entity_Tag	: in    Ada.Tags.Tag
			) return String is abstract;
	-- get the alias for the given entity
	-- for database backend, it's the table name

	---------------------
	-- Store Procedure --
	---------------------

	procedure Store(
				Data_Storage	: in out Data_Storage_Type;
				Entity		: in out Entity_Type'Class
			) is abstract;
	-- store the entity




	--------------------
	-- Load Functions --
	--------------------
	function Load(
				Data_Storage	: in     Data_Storage_Type;
				Filter		: in     Property_Type'Class
			) return KOW_Ent.Entity_Type'Class;
	-- build the query and then return the type
	
	function Load(
				Data_Storage	: in     Data_Storage_Type;
				Query		: in     Queries.Query_Type
			) return KOW_Ent.Entity_Type'Class is abstract;

	function Load(
				Data_Storage	: in     Data_Storage_Type;
				Query		: in     Queries.Joined_Query_Type
			) return KOW_Ent.Entity_Type'Class is abstract;


	

	------------------------
	-- Central Repository --
	------------------------


	type

	type Entity_Registration


end KOW_Ent.Data_Storages;
