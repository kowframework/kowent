




-- This type should be used to 

generic
	type Private_Type is private;
	with function To_String( Element : in Private_Type ) return String;
	Null_Value : constant Numeric_Type;
package KOW_ENT.Generic_Properties is


	type Getter_Type is access function(
			Entity	: in KOW_Ent.Entity_Type'Class
		) return My_Type

	type Setter_Type is access procedure(
			Entity	: in out KOW_Ent.Entity_Type;
			Value	: in Private_Type
		);


	type Property_Type is new KOW_Ent.Entity_Property_Type with record
		Getter	: Getter_Type;
		Setter	: Setter_Type;
	end record;


	overriding
	procedure Set_Property(	
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_Type'Class -- the connection that belongs the query
			);
	-- Set the property from the query into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_Type'Class	-- the connection that belongs the query
			);
	-- Append into a query being created by the main KOW_ent engine.


	overriding
	procedure Set_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			);
	-- Set the property from a String representation of the value
	
	overriding
	function Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String;

	overriding
	function Should_Read( Property : in Property_Type ) return Boolean;
	-- Asks if the value should be set from the database or not
	-- Default :: true
	-- This is here for the Password_Property_Type (that doesn't read from the database)

	overriding
	function Should_Store( Property : in Property_Type; Entity : in Entity_Type'Class ) return Boolean;
	-- asks if the property for the given entity should be stored or no
	-- useful to track if the user password has been changed and need to be stored back or not
	-- Default :: true

end KOW_Ent.Generic_Properties;
