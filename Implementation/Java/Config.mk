JarPrefix := cks-

Components += core
Components += binary text-api text-reader text-writer json
Components += type gen dynamic tool surface workbench test

DEP_binary      := core
DEP_text_api    := core
DEP_text_reader := core text-api
DEP_text_writer := core text-api
DEP_json        := core text-api

DEP_type    := core text-api text-reader
DEP_gen     := core text-api type
DEP_dynamic := core text-api binary type surface
DEP_surface := core text-api text-reader
DEP_tool      := core text-api text-reader text-writer json type gen dynamic surface binary
DEP_workbench := core text-api text-reader text-writer json type dynamic surface binary
DEP_test      := core text-api text-reader text-writer json surface binary tool

LIB_type          := cakoose-util
LIB_gen           := cakoose-util
LIB_tool          := cakoose-util
LIB_workbench     := cakoose-util
LIB_dynamic       := cakoose-util
LIB_test          := cakoose-util
LIB_json          := jackson-core-2.1.1
