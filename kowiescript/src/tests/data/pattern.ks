let how = "awful";

match how {
  when "bad" either "awful" then {
  	print("Not good at all :(");
  }
  when "well" then {
  	print("Great success !!!");
  }
  default {
  	print("What ???");
  }
}