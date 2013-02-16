





                webkit_dom_document_get_element_by_id(self, elementId) {
                    return self["getElementById"]($hs_fromUtf8(elementId));
                };



    var datacon = { value: { constructor: 1
                           , field1: Object } }
    var thunk   = { evaluate: Function }
    var func    = { value: { fun: Function } }

    function force(o) {
      if(o.evaluate) {
        o.value = o.evaluate();
        o.evaluate = null;
      }
      return o.value;
    }

    function fun(f) {
      return { value: { fun: f } }
    }

    function apply1(f, x) {
      return { evaluate:
                 function() {
                   var _f = force(f);
                   return _f.fun(x);
                 }
             }
    }

    var bool = fun(function(c, x, y) {
      var _c = force(c);
      switch(_c.constructor) {
        case 1: // False
          return x;
        case 2: // True
          return y;
      }
    }






