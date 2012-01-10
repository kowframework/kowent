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



package KOW_Ent is




	-----------------------
	-- The Property Type --
	-----------------------



	---------------------------------
	-- The Property Container Type --
	---------------------------------

	type Property_Container_Type is tagged record
		-- the property container type is a stucture with
		-- properties.
		--
		-- it's basically an Entity that doesn't know where 
		-- to be stored
		Properties	: Property_Lists.List;
	end record;






	---------------------------
	-- The Data Storage Type --
	---------------------------

	type Data_Storage_Type is interface;
	-- this is the type that actually handles storing and retrieving data

	type Data_Storage_Ptr is access all Data_Storage_Type'Class;

	procedure Read(
			Data_Storage	: in out Data_Storage_Type;
			Container	: in out Property_Container_Type'Class;
			Query		: in     Query_Type
		) is abstract;
	
	procedure Write(
			Data_Storage	: in out Data_Storage_Type;
			Container	: in     Property_Container_Type'Class
		) is abstract;


	---------------------
	-- The Entity Type --
	---------------------
	
	type Entity_Type is new Property_Container_Type with record
		Data_Storage	: Data_Storage_Ptr;
	end record;

	procedure Load()

end KOW_nt;
