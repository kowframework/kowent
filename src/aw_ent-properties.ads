-- This package contains all from factory supported property type handlers.
-- 
--
-- NOTE :: for now the only type implemented is Unbounded_String
-- TODO :: implement support for more types.


with Aw_Ent;


package Aw_Ent.Properties is

	-------------------------------
	-- Unbounded String property --
	-------------------------------

	type UString_Getter_Type is not null access function(
				Entity	: in Entity'Class
			) return Unbounded_String;
	type UString_Setter_Type is not null access procedure(
				Entity	: in Entity'Class;
				Value	: in Unbounded_String
			);

	type UString_Property_Type is new Entity_Property_Type with private;

	overriding
	procedure Set_Property(	
				Property: in     UString_Entity_Property_Type;		-- the property worker
				Entity	: in out Entity_Type'Class;		-- the entity
				Q	: in out Query_Type'Class		-- the query from witch to fetch the result
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property: in     Entity_Property_Type;		-- the property worker
				Entity	: in out Entity_Type'Class;		-- the entity
				Query	: in out Query_Type'Class		-- the query to witch append the value to insert
			);


	function New_UString_Property(
				Name	: in     String;
				Getter	: in     UString_Getter_Type;
				Setter	: in     UString_Setter_Type
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
private

	type UString_Property_Type is new Entity_Property_Type with record
		Getter : UString_Getter_Type;
		Setter : UString_Setter_Type;
	end record;
end Aw_Ent.Properties;
