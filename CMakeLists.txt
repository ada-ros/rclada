cmake_minimum_required(VERSION 3.5)
project(rclada VERSION 0.1.0)

set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 1)

find_package(rclada_common REQUIRED)
ada_begin_package()

# ROS2 imports
find_package(action_msgs REQUIRED)              # for the thin binding
find_package(rcl REQUIRED)
find_package(rcl_action REQUIRED)
find_package(rcutils REQUIRED)
find_package(rmw REQUIRED)
find_package(std_msgs REQUIRED)                 # Dependency of our test messages
find_package(unique_identifier_msgs REQUIRED)   # Required by actions

# Ada imports
find_package(rosidl_generator_ada REQUIRED)

# Generations of test messages, ROS2 standard
rosidl_generate_interfaces(${PROJECT_NAME}
        msg/Test.msg
        srv/Test.srv
        DEPENDENCIES std_msgs
        )

# We need to import our own messages for use in this same package
ada_import_interfaces(
        ${PROJECT_NAME}
        std_msgs        # we also use some std msgs in selftests
)

# C imports for the Ada side
ada_import_c_libraries(${rcutils_LIBRARIES})
ada_import_c_libraries(${rcl_LIBRARIES})
ada_import_c_libraries(${rcl_action_LIBRARIES})

# Find header folders
ada_find_package_include_dir(_action_msgs_include ${action_msgs_DIR})
ada_find_package_include_dir(_rcl_include ${rcl_DIR})
ada_find_package_include_dir(_rcl_action_include ${rcl_action_DIR})
ada_find_package_include_dir(_rcutils_include ${rcutils_DIR})
ada_find_package_include_dir(_rmw_include ${rmw_DIR})
ada_find_package_include_dir(_unique_identifier_msgs_include ${unique_identifier_msgs_DIR})

# Compile the list of dirs with header files. When installed, its always the
# same /opt/ros/foxy/include
list(APPEND _includes
        ${_rcutils_include}
        ${_rcl_include}
        ${_rcl_action_include}
        ${_rmw_include}
        ${_rosidl_gen_c_include}
#        ${_rosidl_typesupport_interface_include}
        ${_unique_identifier_msgs_include}
        /usr/include
        )

# Bulk addition of headers. See below _extra_headers for a more controlled
# inclusion of needed C headers. This was done out of laziness, but including
# headers in an on-demand fashions is less likely to introduce problems with
# the binding autogenerator. OTOH, if bulk worked, it would be future-proof.
file(GLOB
	_headers
	"${_rcl_include}/rcl/*.h")

# Problematic headers not to process...
list(FILTER _headers EXCLUDE REGEX ".*/rcutils/stdatomic_helper.*")
list(FILTER _headers EXCLUDE REGEX ".*/rcl/rmw_implementation_identifier_check.h")

# Extra headers that we need.
# We don't use the glob tactic as for rcl/*.h because that brings in too much
# standard stuff, some of it not compilable.
set(_extra_headers
        # ACTION MSGS (required by rcl_action)
        ${_action_msgs_include}/action_msgs/msg/detail/goal_info__struct.h
        ${_action_msgs_include}/action_msgs/msg/detail/goal_status__struct.h
        ${_action_msgs_include}/action_msgs/msg/detail/goal_status_array__struct.h
        ${_action_msgs_include}/action_msgs/srv/detail/cancel_goal__struct.h

        # ACTIONS
        ${_rcl_action_include}/rcl_action/action_client.h
        ${_rcl_action_include}/rcl_action/action_server.h
        ${_rcl_action_include}/rcl_action/goal_handle.h
        ${_rcl_action_include}/rcl_action/types.h
        ${_rcl_action_include}/rcl_action/wait.h

        # RCL UTILS
        ${_rcutils_include}/rcutils/allocator.h
        ${_rcutils_include}/rcutils/error_handling.h
        ${_rcutils_include}/rcutils/logging.h
        ${_rcutils_include}/rcutils/time.h
        ${_rcutils_include}/rcutils/types/char_array.h
        ${_rcutils_include}/rcutils/types/rcutils_ret.h
        ${_rcutils_include}/rcutils/types/string_array.h
        ${_rcutils_include}/rcutils/types/string_map.h
        ${_rcutils_include}/rcutils/types/uint8_array.h

        # RMW
        ${_rmw_include}/rmw/init.h
        ${_rmw_include}/rmw/init_options.h
        ${_rmw_include}/rmw/localhost.h
        ${_rmw_include}/rmw/message_sequence.h
        ${_rmw_include}/rmw/names_and_types.h
        ${_rmw_include}/rmw/ret_types.h
        ${_rmw_include}/rmw/security_options.h
        ${_rmw_include}/rmw/serialized_message.h
        ${_rmw_include}/rmw/topic_endpoint_info.h
        ${_rmw_include}/rmw/topic_endpoint_info_array.h
        ${_rmw_include}/rmw/types.h

        # UNIQUE IDENTIFIERS (required by action_msgs)
        ${_unique_identifier_msgs_include}/unique_identifier_msgs/msg/detail/uuid__struct.h
        )

message(STATUS "INCLUDES ${_includes}")
message(STATUS "HEADERS ${_headers} ${_extra_headers}")

ada_generate_binding(
        ada_binding_rcl
        gpr_rcl
        rcl.gpr
        "${_includes}"
        ${_headers}
        ${_extra_headers}
)

# Selftest executables. Some of these depend on the Test.msg/srv.
ada_add_executables(
        gpr_selftest
        gpr_selftest
        bin
        # binaries
        rclada_double_node
        rclada_selftest_dynamic
        rclada_selftest_static
        rclada_test_allocators
        rclada_test_multicore
)

add_dependencies(
        gpr_selftest
        ada_binding_rcl # We need our own RCL binding
)

ada_end_package()
