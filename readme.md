# Fulcro Subscriptions 

See workspaces for a usage example. Read the code to see how it works - there isn't much. 


Future plans
 1. Incorporate a push model for subscriptions. Right  now they depend on short-circuiting the invocation asap while running every subscription function. 
 2. Figure out how to make these more reloadable
 3. Add middleware for renderers. First thing to be added there is middleware to limit how much work can be done per animation frame. Maybe change the return signature of use-sub so that meta (like :invalidated?) can be attached to the result.
 4. Figure out a really good solution to workspaces. Right now since there is only one global register for subscriptions, having 2 workspaces open causes clashes. 
 5. Probbably easy, but add a dynamic var for app so the initial value can be auto-filled. Won't really play nice with workspaces though...
 
 
 I'm sure more will come later.

