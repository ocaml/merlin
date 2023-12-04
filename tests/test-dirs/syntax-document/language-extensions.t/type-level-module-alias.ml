module MyLib__A: sig
  type a_thing = int
  val a_value: a_thing
end

module MyLib: sig
  module A = MyLib__A
end