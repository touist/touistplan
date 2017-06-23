#!/usr/bin/env bash

# @author Djamila BAROUDI baroudi.d7@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# This script generate the right TOUIST syntax out of the given PDDL files.

banner=`cat <<-EOF
 _______  _______  _______    _______  _______  ___      __   __  ___   __    _  _______ 
|       ||   _   ||       |  |       ||       ||   |    |  | |  ||   | |  |  | ||       |
|  _____||  |_|  ||_     _|  |  _____||   _   ||   |    |  |_|  ||   | |   |_| ||    ___|
| |_____ |       |  |   |    | |_____ |  | |  ||   |    |       ||   | |       ||   | __ 
|_____  ||       |  |   |    |_____  ||  |_|  ||   |___ |       ||   | |  _    ||   ||  |
 _____| ||   _   |  |   |     _____| ||       ||       | |     | |   | | | |   ||   |_| |
|_______||__| |__|  |___|    |_______||_______||_______|  |___|  |___| |_|  |__||_______| 
EOF
`
#######################################
#               Arguments
#######################################
pddl_file_domain=${1}
pddl_file_problem=${2}
pddl_file_constraints=${3}
pddl_depth=${4}
#######################################
#               Variables
#######################################
touist_tmp_file='tmp.touistl'
touist_binary_path=$(which touist)
touist_constraints_file='constraint.cst'
pddl2touistl_binary_path='pddl2touistl'
#######################################
#             Utility functions  
#######################################
# Basic usage function
function display_usage() {
    echo "Usage: `basename $0` domain.pddl problem.pddl constraints.pddl depth [-h/--help]"
}
# Detects the OS Kernel name
function detect_client_info() {
  local kernel=$(uname -s)
  case "${kernel}" in
    Darwin)
      CLIENT_PLATFORM="darwin"
      ;;
    Linux)
      CLIENT_PLATFORM="linux"
      ;;
    *)
      echo "[-] Unknown, unsupported platform: ${kernel}." >&2
      echo "[-] Supported platforms: Linux, Darwin." >&2
      exit 1
  esac
}

# Verifies PDDL arguments
function verify_pddl_domain() {
    if [[ -z "${pddl_file_domain}" ]]
    then
        display_usage    
        echo '[-] PDDL file for the domain is missing'
        echo '[-] Bailing out'
        exit 2
    fi
    local _command_result_content=`cat "${pddl_file_domain}" | grep -v "^$" | head -n 1 | grep -i "(domain"`
    if [[ $? -le 0 ]] && [[ -z ${_command_result_content} ]]; then
        echo "[-] ${pddl_file_domain} is not a valid PDDL domain file."
        echo '[-] Bailing out'
        exit 3
    fi
}

function verify_pddl_problem() {
    if [[ -z "${pddl_file_problem}" ]]
    then
        display_usage
        echo '[-] PDDL file for the domain is missing'
        echo '[-] Bailing out'
        exit 4
    fi
    local _command_result_content=`cat "${pddl_file_problem}" | grep -v "^$" | head -n 1 | grep -i "(problem"`
    if [[ $? -le 0 ]] && [[ -z ${_command_result_content} ]]; then
        echo "[-] ${pddl_file_problem} is not a valid PDDL problem file."
        echo '[-] Bailing out'
        exit 5
    fi
}

function verify_pddl_depth() {
    if [[ -z "${pddl_depth}" ]]
    then
        echo '[-] PDDL depth is missing'
        display_usage
        echo '[-] Bailing out'
        exit 6
    fi
    # Checks if the depth variable is well formed integer number
    number_regex='^[0-9]+$'
    if ! [[ ${pddl_depth} =~ ${number_regex} ]] ; then
        echo "[-] PDDL depth is not a number NAN"
        echo '[-] Bailing out'
        exit 7
    fi
    # Checks if is positive integer number
    if [[ "${pddl_depth}" -le 0 ]]
    then
        echo '[-] PDDL depth must be positive'
        echo '[-] Bailing out'
        exit 8
    fi
}

function is_file_exits() {
	local file="$1"
	[[ -f "$file" ]] && return 0 || return 1
}

function check_dependepcies() {
    # Touist must be present as path variable
    if [[ ! $(which touist) ]]; then
        echo '[-] touist binary is missing.'
        echo '[-] Check https://opam.ocaml.org/packages/touist/touist.3.1.0/'
        echo '[-] or install via: opam install touist'
        echo '[-] Bailing out'
        exit 9
    fi
    # Checks if the pddl2touistl exists and if it is an executable
    if (is_file_exits ${pddl2touistl_binary_path}) 
    then
        if [[ ! -x ${pddl2touistl_binary_path} ]]; then
            chmod +x ${pddl2touistl_binary_path}
        fi
    fi
}

function build_touist_from_pddl() {
    # Delete touist file
    /bin/rm ${touist_tmp_file}
    local pddl2touistl_result=`./pddl2touistl ${pddl_file_domain} ${pddl_file_problem} ${pddl_file_constraints} ${pddl_depth}`
    echo "[+] Saving touist file under ${touist_tmp_file}..."
    if (is_file_exits ${touist_constraints_file}) 
    then
        cat ${touist_constraints_file} > ${touist_tmp_file}
        # Append pddl to touist result
        echo "${pddl2touistl_result}" >> ${touist_tmp_file}
    else
        echo "${pddl2touistl_result}" > ${touist_tmp_file}
        echo "[-] Constraints file does not exist at ${touist_constraints_file}"
    fi
}

function solve_touist() {
    local _done=0
    while [[ ${_done} -ne 1 ]]
    do
        echo "[+] Computing the depth ${pddl_depth}..."
        echo "###############################"
        build_touist_from_pddl
        # Touist
        echo "[+] Computing touist file..."
        local _command_touist="${touist_binary_path} ${touist_tmp_file} --solve"
        local _result_touist=$(${_command_touist})
        # Prints out the results if there is any
        echo "${_result_touist}"
        # Tests if touist did not solve this depth level
        #NB: touist output "empty" if it is unsat in the standard output
        local _test_result=`echo ${_result_touist} | head -n 1 | sed -e 's/^[[:space:]]*//'`
        if [[ -z ${_test_result} ]]; then
            # Increments depth counter
            pddl_depth=$(expr ${pddl_depth} + 1)
            echo "[+] Computing the next generation with depth ${pddl_depth}"
        else
            _done=1
            echo "[+] Done."
        fi
    done
}

#if ( is_file_exits "$1" )
#then
# echo "File found"
#else  
# echo "File not found"
#fi
#######################################
#               Main
#######################################

# Invoke usage
# Calls usage() function if arguments are not supplied
# If less than two arguments are supplied, display usage 
if [  $# -le 2 ] 
then 
    display_usage
    exit 10
fi 
# Checks whether user had supplied -h or --help . If yes display usage 
if [[ ( $# == "--help") ||  $# == "-h" ]] 
then 
    display_usage
    exit 11
fi
#######################################
#               Banner
#######################################
echo "${banner}"
echo ''
echo "##############################################################################"
echo "[+] Detecting client machine information..."
detect_client_info

echo "[+] Checking dependencies..."
check_dependepcies

# Loading arguments
echo "[+] Loading arguments..."
verify_pddl_domain
verify_pddl_problem
verify_pddl_depth

echo "[+] Initialization done with success."

echo "[+] Starting computations..."
echo "[+] Launching pddl2touist with ${pddl_file_domain} and ${pddl_file_problem} at depth ${pddl_depth}"
echo "##############################################################################"
# Run touist solving
solve_touist