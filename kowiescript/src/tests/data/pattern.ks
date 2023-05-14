let how = "well";

match how {
  when "bad" either "awfull" then {
  	print("Not good at all :(");
  }
  when "well" then {
  	print("Great success !!!");
  }
  default {
  	print("What ???");
  }
}