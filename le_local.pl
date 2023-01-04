/* le_local: a prolog module for LE handling of a local filesystem.

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*/

:- module(le_local, 
    [load_file_module/3, 
     this_capsule/1,
     portray_clause_ind/1
    ]).

load_file_module(FileName, FileName, _) :-
   load_files([FileName], [module(FileName)]). 

this_capsule(M) :-
   current_module(M). 

portray_clause_ind(Clause) :- 
    portray_clause(Clause). 