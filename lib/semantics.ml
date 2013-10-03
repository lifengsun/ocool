let run classes =
  let all_classes = Basic_classes.basic_classes @ classes in
  let inherit_tree = Inherit_tree.build all_classes in
  let method_env = Method_env.build all_classes in
  Type_check.run all_classes inherit_tree method_env;
  Inherit_tree.print inherit_tree;
  Method_env.print method_env;
  all_classes
