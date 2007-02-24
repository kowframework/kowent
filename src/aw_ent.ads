-- main package for Aw_Ent.
-- 
-- IT'S STILL ONLY FOR PLANNING PURPOSE! DON'T TRUST IT! IT LIES!
--
-- author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 
-- createdAt 2007-02-21
--
-- Repository information
-- $Date$
-- $Revision$
-- $Author$


package Aw_Ent is

	type Entity_Interface is abstract tagged private;

private

	type Entity_Map is record
		id: integer;
	end record;

	type Entity_Interface is abstract tagged private record
	end record;


end Aw_Ent;
