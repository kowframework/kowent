------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Entity                             --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- KOW_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------



-- This is the main package for entity relation
-- Here is where the relation is tracked down into an entity
--
-- The relation can be maped as an entity property and when that's so the user
-- can instanciate a generic package for handling querying related entities.
--
-- @author Marcelo C. de Freitas <marcelo@kow.com.br>

--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;


---------------
-- Ada Works --
---------------
with KOW_Ent;
with KOW_Ent.Properties;
with KOW_Ent.Query_Builders;



package KOW_Ent.Relations is



	generic
		type From_Entity_Type is new Entity_Type with private;
		type To_Entity_Type is new Entity_Type with private;
		with procedure Set_Foreign_Key( Entity : in out To_Entity_Type; Key_From : in KOW_Ent.Entity_Type'Class );
	package One_to_Many_Relation_Handlers is
		
		package Related_Entity_Query_Builders is new KOW_Ent.Query_Builders( Entity_Type => To_Entity_Type );

		function get_All( Entity : in From_Entity_Type ) return Related_Entity_Query_Builders.Entity_Vectors.Vector;
		
		function get_First( Entity : in From_Entity_Type ) return To_Entity_Type;

		procedure Store_All(
				Entity			: in out From_Entity_Type;
				Related_Entities	: in out Related_Entity_Query_Builders.Entity_Vectors.Vector
			);
	end One_To_Many_Relation_Handlers;



private
	

end KOW_Ent.Relations;