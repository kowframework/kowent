with APQ;


package body KOW_ENT.Properties.Generic_Date is


	------------------
	-- The Property --
	------------------


	overriding
	procedure Set_Property(	
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_Type'Class -- the connection that belongs the query
			) is
		-- Set the property from the query into the Entity.
		Column : String := To_String( Property.Column_Name );
		Index  : APQ.Column_Index_Type := APQ.Column_Index( Q, Column );
	begin
		Property.Setter.all(
				Entity,
				Value( Q, Index )
			);
		exception
			when APQ.Null_Value =>
				Property.Setter.all(
						Entity,
						Null_Value
					);
	end Set_Property;



	overriding
	procedure Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_Type'Class	-- the connection that belongs the query
			) is
		-- Append into a query being created by the main KOW_ent engine.
	begin
		Append( Query, Property.Getter.all( Entity ) );
	end Get_Property;



	overriding
	procedure Set_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is
		-- Set the property from a String representation of the value
	begin
		Property.Setter.all( Entity, From_String( Value ) );
	end Set_Property;



	overriding
	function Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is
	begin
		return To_String( Property.Getter.all( Entity ) );
	end Get_property;
end KOW_Ent.Properties.Generic_Date;
