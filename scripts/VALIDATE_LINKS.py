#!/usr/bin/env python3
"""
Link validation script for documentation files
Validates all markdown cross-references and checks for broken links
"""

import os
import re
from pathlib import Path

def find_all_markdown_files():
    """Find all markdown files in project"""
    md_files = []
    skip_dirs = {'.backup_20251031_192302', '.git', 'node_modules', '.backup_links_20251031_204331'}
    
    for root, dirs, files in os.walk('.'):
        dirs[:] = [d for d in dirs if d not in skip_dirs and not d.startswith('.')]
        for file in files:
            if file.endswith('.md'):
                md_files.append(os.path.join(root, file))
    
    return sorted(md_files)

def extract_links_from_file(filepath):
    """Extract all markdown links from a file"""
    links = []
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Find markdown links: [text](path)
    pattern = r'\[([^\]]+)\]\(([^)]+\.md)\)'
    matches = re.finditer(pattern, content)
    
    for match in matches:
        text, link = match.groups()
        links.append({
            'file': filepath,
            'text': text,
            'link': link,
            'type': 'markdown'
        })
    
    # Find inline backtick references: `FILENAME.md`
    pattern = r'`([^`]*\.md)`'
    matches = re.finditer(pattern, content)
    
    for match in matches:
        link = match.group(1)
        # Only add if it looks like a file reference
        if '/' in link or not '/' in filepath.split(os.sep)[-1]:
            links.append({
                'file': filepath,
                'text': link,
                'link': link,
                'type': 'backtick'
            })
    
    return links

def resolve_link(file_context, link):
    """Resolve a relative link from a file context"""
    file_dir = os.path.dirname(file_context)
    
    # Handle relative paths
    if link.startswith('../'):
        # Go up directories
        parts = link.split('/')
        current = file_dir
        for part in parts:
            if part == '..':
                current = os.path.dirname(current)
            elif part:
                current = os.path.join(current, part)
        return current
    elif link.startswith('./'):
        return os.path.join(file_dir, link[2:])
    else:
        # Same directory
        return os.path.join(file_dir, link)

def validate_links():
    """Validate all links in documentation"""
    all_files = find_all_markdown_files()
    print(f"Found {len(all_files)} markdown files\n")
    
    # Create mapping of all files
    file_map = {os.path.normpath(f): f for f in all_files}
    
    broken_links = []
    valid_links = 0
    invalid_links = 0
    
    # Check each file's links
    for md_file in all_files:
        links = extract_links_from_file(md_file)
        
        for link_info in links:
            link = link_info['link']
            
            # Skip external links
            if link.startswith('http'):
                continue
            
            # Resolve the link
            resolved = resolve_link(md_file, link)
            resolved = os.path.normpath(resolved)
            
            # Check if file exists
            if resolved in file_map or os.path.exists(resolved):
                valid_links += 1
            else:
                invalid_links += 1
                broken_links.append({
                    'source': md_file,
                    'link': link,
                    'resolved': resolved,
                    'type': link_info['type']
                })
    
    # Print results
    print(f"=== LINK VALIDATION RESULTS ===")
    print(f"Valid links: {valid_links}")
    print(f"Invalid/Broken links: {invalid_links}")
    print(f"Success rate: {100 * valid_links / (valid_links + invalid_links):.1f}%\n")
    
    if broken_links:
        print(f"Found {len(broken_links)} broken links:\n")
        for broken in broken_links[:20]:  # Show first 20
            print(f"File: {broken['source']}")
            print(f"  Link: {broken['link']}")
            print(f"  Resolved to: {broken['resolved']}")
            print()
        
        if len(broken_links) > 20:
            print(f"... and {len(broken_links) - 20} more broken links")
    else:
        print("No broken links found! ✓")
    
    return len(broken_links) == 0

if __name__ == '__main__':
    success = validate_links()
    exit(0 if success else 1)
