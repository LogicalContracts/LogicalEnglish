/* le_swish: a prolog module for LE handling of the gitty filesystem.

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*/

:- module(le_swish, 
    [load_file_module/3, 
     this_capsule/1
    ]).

:- multifile sandbox:safe_primitive/1.

:- use_module(library(pengines)).
:- use_module(kp_loader).

load_file_module(FileName, ModuleName, Flag) :-
   load_named_file(FileName, ModuleName, Flag). 

this_capsule(M) :-
   pengine_self(M).


