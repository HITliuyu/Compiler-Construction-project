/*
 * Copyright (c) 2013, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS """AS IS""" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * */

package de.tuberlin.uebb.comp1.moc
import scala.collection.immutable.HashMap

/** The μ-Opal context checker */

object ContextChecker {
  import AbstractSyntax._ 

  case class Context(decls : Map[String, Lhs], vars : Map[String, Type]){

   // Utility def for adding to the context
    def ++(list_decls : List[(String, Type)]): Context = {
      if (list_decls.isEmpty) return this
      (this + (list_decls.head._1, list_decls.head._2)) ++ list_decls.tail
    }
    
    def +(id : String, d : Lhs): Context = {
      Context(this.decls + ((id, d)), this.vars)
    }
    
    def +(id : String, t : Type): Context = {
      Context(this.decls, this.vars + ((id, t)))
    }

  }
  /**
   * Starts context check for μ-Opal
   * @param prog Complete program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return A list of error messages
   */
  
// List of primitive declarations 
    val prim_decls = List(
    ("add", Decl("add",List(Param("",Nat), Param("",Nat)), Nat, Global)),
    ("sub", Decl("sub",List(Param("",Nat), Param("",Nat)), Nat, Global)),
    ("mul", Decl("mul",List(Param("",Nat), Param("",Nat)), Nat, Global)),
    ("div", Decl("div",List(Param("",Nat), Param("",Nat)), Nat, Global)),
    ("eq", Decl("eq",List(Param("",Nat), Param("",Nat)), Bool, Global)),
    ("lt", Decl("lt",List(Param("",Nat), Param("",Nat)), Bool, Global)),
    ("and", Decl("and",List(Param("",Bool), Param("",Bool)), Bool, Global)),
    ("or", Decl("or",List(Param("",Bool), Param("",Bool)), Bool, Global)),
    ("not", Decl("not",List(Param("",Bool)), Bool, Global))
    )
    
  def check(prog : Prog, opts : Options) : Option[List[Diag]] = {

    val init_context = new Context(new HashMap[String, Decl]() ++ prim_decls,new HashMap[String, Type]())

    val (context_lhs, err_lhs) = checkLhs(prog.d, init_context, List())
    val (context_expr, err_expr) = checkExprs(prog.d, context_lhs, List())

    if(err_lhs.isEmpty && err_expr.isEmpty)
      None
    else
      Some(err_lhs ++ err_expr)
  }
  
  def checkLhs(defs : List[Def], context : Context, err: List[Diag]) : (Context, List[Diag]) = {
    
    if(defs.isEmpty)
      if(!context.decls.contains("MAIN"))
        (context, err :+ Diag("MAIN not found.", Global))
      else
      (context, err)
    else
    {
      
      val (context2, err2) = defs.head.lhs match {
        
        case MainDecl(t, pos) => {
          if(context.decls.contains("MAIN"))
            (context, err :+ Diag("Multiple instances of MAIN found.", pos))
          else
            (context + ("MAIN", MainDecl(t, pos)), err)
        }
        case Decl(id, params, t, pos) => {
        
          val param_ids = params.map { p => p.id}
          
          val param_id_list = param_ids.diff(param_ids.distinct).distinct
       
          val param_err = if (param_id_list.length > 0)
									Diag("Parameter names should be disjointed. Found duplicate entries for: " + (param_id_list mkString (", ")) + " in function: '" + id + "'.", pos) :: Nil
											else
													List()
        
          if(context.decls.contains(id) && !prim_decls.map(p => p._1).contains(id))
            (context, err ++ param_err :+ Diag("Multiple instances of '"+ id +"' found.", pos))
          else if(context.decls.contains(id) && prim_decls.map(p => p._1).contains(id))
            (context, err ++ param_err :+ Diag("Multiple instances of '"+ id +"' found. '" + id + "' is a primitive function.", pos))
          else
            (context + (id, Decl(id, params, t, pos)), err ++ param_err)
        }
        
      }
      
       checkLhs(defs.tail, context2, err2)
    }
  }
  
  def checkExprs(defs : List[Def], context : Context, err: List[Diag]) : (Context, List[Diag]) = {
    
    
    if(defs.isEmpty)
      (context, err)
    else
    {
      val err2 = defs.head.lhs match {
        // checks MainDecl for errors given the current context
        case MainDecl(t1, pos) => {
          val (t2, error_list) = typeOf(defs.head.expr, context)
          
          val type_error = if(t1 ~=~ t2)
              List()
            else
              Diag("Type Mismatch: MAIN requires '" + t1 + "' but returns '" + t2 + "' instead.", pos)::Nil
            
           (error_list ++ type_error)
        }
        
        case Decl(id, params, t1, pos) => {
          // checks Decl for errors after adding its parameters to the context
          val (t2, error_list) = typeOf(defs.head.expr, context ++ params.map { p => (p.id, p.t) })

          // checks if the declared type is the same as the return type
          val type_error = if(t1 ~=~ t2)
              List()
            else
              Diag("Type Mismatch: DEF requires '" + t1 + "' but returns '" + t2 + "' instead.", pos)::Nil
            
           (error_list ++ type_error)
        }
        
      }
    
      checkExprs(defs.tail, context, err ++ err2)
    }
  }
  
  def typeOf(expr : Expr, context : Context) : (Type, List[Diag]) = {
  
    expr match {
      case Number(_) => (Nat, List())
      
      case True | False => (Bool, List())
      
      case Variable(id, pos) => {
        if(context.vars.contains(id))
          (context.vars(id), List())
        else
          (Unknown, Diag("Undeclared Variable '" + id + "'.", pos)::Nil)
      }
      
      case Function(id, args, pos) => {
        if(context.decls.contains(id))
        {
          context.decls(id) match {
           case Decl(id, args_def, t, _) => {
              
             // creates a list containing the argument types and eventual errors
             val fun_types_list = args.map { e => typeOf(e, context) }
             val fun_types = fun_types_list.map(e => e._1)
             val fun_types_errs = fun_types_list.map(e => e._2).flatten
             
             // creates a list containing the required param types
             val type_list_def = args_def.map { p => p.t}
             
             // checks for incorrect number of arguments
             val type_length_errs = if(args.length == args_def.length)
                   List()
                 else
                   Diag("Arguments Mismatch: Expected " + args.length + " arguments, got " + args_def.length + " instead.", pos)::Nil
                   
             // creates a list of errors, if there are mimatches between the types
             // From: https://stackoverflow.com/questions/17812486/comparing-items-in-two-lists
             val type_errs = type_list_def.zip(fun_types).filter(ts => !(ts._1 ~=~ ts._2)).map(
                 ts => Diag("Type Mismatch: Expected type '" + ts._1 + "', found '" + ts._2 + "' instead.", pos))
             
             (t, fun_types_errs ++ type_length_errs ++ type_errs)
            
           }
            case _ => (Unknown, Diag("Invokation error for '" + id + "'.", pos)::Nil)
         }
        }
        else
          (Unknown, Diag("Undeclared Definition '" + id + "'.", pos)::Nil)
      }
      
      case IfThen(e1, e2, pos) => {
        // checks the types of e1 and e2
        val (e1_type, e1_err) = typeOf(e1, context)
        val (e2_type, e2_err) = typeOf(e2, context)
        
        // checks if the first expression is Bool (well-typed)
        val type_error = if(e1_type ~=~ Bool)
              List()
            else
              Diag("Incorrect assertion type: Expected type Bool, found '" + e1_type +"' instead.", pos)::Nil
        
        (e2_type, e1_err ++ e2_err ++ type_error)
      }
      
      case IfThenElse(e1, e2, e3, pos) => {
        // checks the types of e1, e2 and e3
        val (e1_type, e1_err) = typeOf(e1, context)
        val (e2_type, e2_err) = typeOf(e2, context)
        val (e3_type, e3_err) = typeOf(e3, context)
        
        // checks if the first expression is Bool (well-typed)
        val type_error = if(e1_type ~=~ Bool)
              List()
            else
              Diag("Incorrect assertion type: Expected type Bool, found '" + e1_type +"' instead.", pos)::Nil
        
        if(e2_type ~=~ e3_type)
          (e2_type, e1_err ++ e2_err ++ e3_err ++ type_error)
        else
          (e2_type, e1_err ++ e2_err ++ e3_err ++ type_error :+ Diag("Type Mismatch: IF expression has type '" + e2_type + 
              "' and ELSE expression has type '" + e3_type + "'.", pos))
      }
    }
  }
  
}
