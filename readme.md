# Fulcro Subscriptions 

See workspaces for a usage example. Read the code to see how it works - there isn't much. 

I use this daily for company software, and potentially for some other OSS work. However, that does not mean that I have fully committed to this model. I may end up making several breaking changes to the code. In clojure fashion I will make copy namespaces with an alternate name. 


Future plans
 1. Incorporate a push model for subscriptions. Right now they depend on short-circuiting the invocation asap while running every subscription function. Will require some more external tooling to make it work 100% right. 
 2. Figure out how to make these more reloadable. Hooks in general are hard to add to a component. React counts the number of hooks per component and will have a fit if you change them at runtime. 
 3. Add middleware for renderers. First thing to be added there is middleware to limit how much work can be done per animation frame. Maybe change the return signature of use-sub so that meta (like :invalidated?) can be attached to the result.
 4. Figure out a really good solution to workspaces. Right now since there is only one global register for subscriptions, having 2 workspaces open causes clashes. 
 5. Probably easy, but add a dynamic var for app so the initial value can be auto-filled. Won't really play nice with workspaces though...
 6. Better examples
 7. Once everything is more solid, a blog post is in order describing use-cases, how it works, and the layers of abstraction applied. The current design makes it quite easy to scale from a single function as your subscription to complicated partially filled trees of functions with short circuit invocations. 
 
 
 I'm sure more will come later.

