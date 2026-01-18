let students = [];

// 1. Load Data from JSON
async function loadStudents() {
    try {
        const response = await fetch('students.json');
        students = await response.json();
        displayStudents(students);
    } catch (error) {
        console.error("Error loading student data:", error);
    }
}

// 2. Render Student Cards
function displayStudents(data) {
    const grid = document.getElementById('studentGrid');
    grid.innerHTML = data.map(s => `
        <div class="card">
            <h2>${s.name}</h2>
            <p><strong>Domain:</strong> ${s.domain} | <strong>CGPA:</strong> ${s.cgpa}</p>
            <div class="tag-container">
                ${s.skills.map(skill => `<span class="tag">${skill}</span>`).join('')}
            </div>
            <p><small>${s.summary}</small></p>
            <p><strong>Experience:</strong> ${s.experience}</p>
            <a href="${s.cv_link}" target="_blank" class="cv-btn">Download CV</a>
        </div>
    `).join('');
}

// 3. Filtering Logic
function filterData() {
    const domainVal = document.getElementById('domainFilter').value;
    const cgpaVal = parseFloat(document.getElementById('cgpaFilter').value) || 0;
    const searchVal = document.getElementById('skillSearch').value.toLowerCase();

    const filtered = students.filter(s => {
        const matchesDomain = domainVal === 'all' || s.domain === domainVal;
        const matchesCGPA = s.cgpa >= cgpaVal;
        const matchesSearch = s.skills.some(sk => sk.toLowerCase().includes(searchVal)) || 
                              s.tools.some(t => t.toLowerCase().includes(searchVal));
        
        return matchesDomain && matchesCGPA && matchesSearch;
    });

    displayStudents(filtered);
}

// Event Listeners
document.getElementById('domainFilter').addEventListener('change', filterData);
document.getElementById('cgpaFilter').addEventListener('input', filterData);
document.getElementById('skillSearch').addEventListener('input', filterData);

// Initial Load
loadStudents();