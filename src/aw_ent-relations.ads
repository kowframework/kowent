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
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
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
with Aw_Ent;
with Aw_Ent.Properties;



package Aw_Ent.Relations is


	type Relation_Type is ( ONE_TO_ONE, ONE_TO_MANY, MANY_TO_MANY );

	type Foreign_Key is record
		Related_Entity : Ada.Tags.Tag;
		-- the entity to whom it's the owner of this FK is related.

		Relation : Relation_Type;
		-- how the entities are related (from this one to the other one)


		Helper_Table : Unbounded_String;
		-- if /= "" maps to a table used to help the relation.
		-- usefull in MANY_TO_MANY relations but can be used to help other times.


		This_ID_Column : Unbounded_String;
		-- the column name for the ID of this entity
		-- ignored when Helper_Table = ""

		Other_ID_Column : Unbounded_String;
		-- the column name for the ID of the related entity.
	end record;


	package Foreign_Key_Lists is new Ada.Containers.Doubly_Linked_Lists(
				Element_Type	=> Foreign_Key
			);


	generic
		type From_Entity_Type is new Entity_Type with private;
		type To_Entity_Type is new Entity_Type with private;
		Relation : Relation_Type;
	package Relation_Handler is
		
		package Related_Entity_Vectors is new Ada.Containers.Vectors(
					Index_Type	=> Natural,
					Element_Type	=> To_Entity_Type
				);

		function get_All( Entity : in From_Entity_Type ) return Related_Entity_Vectors.Vector;
		
		function get_First( Entity : in From_Entity_Type ) return To_Entity_Type;

	end Relation_Handler;



private
	
	protected Key_Registry is


end Aw_Ent.Relations;
