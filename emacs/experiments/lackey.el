;;----------------------------------------------------------------------
;; lackey.el
;; adapted by Mike Mattie
;; from else-mode.el
;;----------------------------------------------------------------------

(defun lackey-mode ()
  ;; setup the buffer local variables

  ;; lackey-overlay

  ;; the lackey-overlay table tracks all the overlays in the buffer.

  ;; This is the core data structure behind the API.

  ;; design considerations: lifetime

  ;; some things will need to live forever, only the actual code behind
  ;; the overlay can compute lifetime.

  ;; the persistence needs to be entirely orthogonal.

  ;; use assemblies and parts as terminology.

  ;; assemblies assemble the environment, and provide signal delivery. parts receive
  ;; signals and respond to them.

  ;; the environment is created with let, a hook provides the signal receiver.

  ;; the relationships and graphs are like lines on which ants or worker routines
  ;; can travel.

  ;; what is propogation ? that must be clearly defined. The creation of a container
  ;; such as a overlay or text property is trivial implementation. What are the rules
  ;; for propogating ? what kind of rules can be made ?

  ;; by default values would not propogate.

  ;; propogate is the same as auto-substitute, just a different name for the same
  ;; sort of behavior.

  ;; need propogate arrays. These arrays would be indexed by a unique key, each
  ;; entry would be a list of all the other members of this set.

  ;; all constructs need to handle a kill signal intelligently. First principle is
  ;; simply to delete more, when a construct becomes invalid it should delete until
  ;; validity is reached. If it dies entirely it needs to propogate that signal. we
  ;; always need to be able to find a parent construct if there is one.

  ;; the assembly should be the initial receiver of a signal. what sort of information
  ;; should be in a signal ?

  ;; the parts affected by the signal.

  ;; when a assembly is a part of another assembly it is a part, how is that relationship
  ;; defined ? )

  ;; would it not be easy for the propogation of a signal to kill something that was referenced ?

  ;; what propogation should really do is construct a new graph of the assembly with the signal
  ;; pass/response. The new construct tree should then be generated from the information
  ;; in the original form.

  ;; This would make sure that objects/annotations could not be deleted and then referenced
  ;; or needed.

  ;; propogation signals help compute a new graph. edit signals compute a new text region.

  ;; it is possible for this mode to write comment prefixed data structures. these sections
  ;; could be turned invisible. additionally a document mode could be provided.

  ;; there is one invisble object, the root of a assembly. This can receive signals like any
  ;; other, or maybe it is special and receives all signals , a broadcast relay that repeats
  ;; the signal to all the parts ? it is invisible, but it can
  ;; receive a create signal , that is how a assembly name is created.

  ;; the propogation signal could also be called the planning signal. When looked at this way,
  ;; propogating symbols in two stages is introducing a planning stage where the validity and
  ;; extent of the change can be planned, and the execution of the change could be made synchronous ?,
  ;; part of undo ?

  ;; user input is either mapped to a command, or self insert. This works a bit like the keymap,
  ;; the signal is one of two types, user generated, or command generated. user generated always
  ;; gets some input, like characters to auto-substitute.

  ;; any time the text is changed without a input character of some sort from the user it is
  ;; considered a command change. Since it is undesirable to constrain the applicablity of
  ;; commands, to remain completely orthogonal to the existing command set, this is why
  ;; all assemblies must handle delete gracefully.

  ;; when treating an assembly as a part the singals could simply be delivered to the special
  ;; root so that a delete command, or a expand command would be delivered to all.

  ;; in fact the propogation mechanism could be called emitters and recievers. the emitters
  ;; would be programmable for different propogation mechanisms such as auto-repeat etc.

  ;; a "broadcast" emitter would repeat the signal to all of it's siblings.

  ;; the emitters/receivers would maintain their own data structures. This is a beautiful
  ;; abstraction that renders the propogation mechanisms orthogonally elegant to the
  ;; implementation of dependency, linkage etc.

  ;; templating facilities could implement or use both emitters and receivers.

  ;; the planning phase should be implemented as a sort of copy and modify. If history
  ;; is maintained then signal rollbacks could be implemented to provide signal rollbacks.
  ;; is this necessary ?

  ;; menu's or special key actions could be implemented as a receiver.

  ;; the before/after change should be implemented as builtin provided emitters.

  ;; there should be a orthogonal tagging of symbols for persistence so that persistence
  ;; does not need to be implemented with awareness or involvement with the objects
  ;; it is saving and restoring.

  ;; how does the overlay connect to the emitter types ? First stab is that I will need
  ;; a buffer global table of the emitters. Each overlay will then have a lackey handler,
  ;; and a index into the emitter array. Once the basic handler activates the first
  ;; emitter, the polypmorphism of the emitter type will be expressed.
  )
