package RDF::RDFa::Parser::Profile::RDF;

use File::Spec;
use HTTP::Cache::Transparent;
use LWP::UserAgent;
use RDF::RDFa::Parser;
use RDF::RDFa::Parser::Config;
use RDF::Trine;

use base qw(RDF::RDFa::Parser::Profile);
use common::sense;
use 5.008;

our $VERSION = '1.092';

BEGIN
{
	HTTP::Cache::Transparent::init({BasePath => File::Spec->tmpdir.'/cache/'});
}

sub new
{
	my ($class, $uri, $parser) = @_;
	
	my $ua       = $parser->{'options'}->lwp_ua;
	my $accept   = 'application/rdf+xml, text/turtle, application/x-rdf+json, '.$ua->default_header('Accept');
	my $response = $ua->get($uri, Accept=>$accept);
	
	return $class->new_from_response($response, $parser);
}

sub new_from_response
{
	my ($class, $response, $parser) = @_;
	
	my $model;
	
	if ($response->code == 200)
	{
		my $trine;
		
		if ($response->content_type eq 'application/rdf+xml')
			{ $trine = 'rdfxml'; }
		elsif ($response->content_type eq 'application/json')
			{ $trine = 'rdfjson'; }
		elsif ($response->content_type eq 'application/x-rdf+json')
			{ $trine = 'rdfjson'; }
		elsif ($response->content_type eq 'text/n3')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'text/turtle')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'application/turtle')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'application/x-turtle')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'text/plain')
			{ $trine = 'ntriples'; }

		if (defined $trine)
		{
			eval
			{
				$model = RDF::Trine::Model->temporary_model;
				my $parser = RDF::Trine::Parser->new($trine);
				$parser->parse_into_model(
					$response->base, $response->decoded_content, $model);
			};
			return undef if $@;
		}
		else
		{
			my $hostlang = RDF::RDFa::Parser::Config->host_from_media_type($response->content_type);
		
			if (defined $hostlang)
			{
				eval 
				{
					my $parser = RDF::RDFa::Parser->new(
						$response->decoded_content,
						$response->base,
						RDF::RDFa::Parser::Config->new($hostlang),
						);
					$parser->consume;
					$model = $parser->graph;
				};
				return undef if $@;
			}
			else
			{
				return undef;
			}
		}
	}
	else
	{
		return undef;
	}

	if (defined $model)
	{
		return $class->new_from_model($model, $parser);
	}
	
	return undef;
}

sub new_from_model
{
	my ($class, $model, $parser) = @_;

	my $self = bless { definitions=>0, prefixes=>[], terms=>[] }, $class;
		
	my $term_results   = $model->get_pattern( $self->_term_pattern );
	while (my $row = $term_results->next)
	{
		next unless $row->{'term'}   && $row->{'term'}->is_literal;
		next unless $row->{'uri'}    && $row->{'uri'}->is_literal;
		
		$self->{definitions}++;
		push @{ $self->{'terms'} },
			[
				$row->{'term'}->literal_value,
				$row->{'uri'}->literal_value,
				0,
				'*',
			], [
				$row->{'term'}->literal_value,
				$row->{'uri'}->literal_value,
				1,
				'*',
			];
	}
	
	my $prefix_results = $model->get_pattern( $self->_prefix_pattern );
	while (my $row = $prefix_results->next)
	{
		next unless $row->{'prefix'} && $row->{'prefix'}->is_literal;
		next unless $row->{'uri'}    && $row->{'uri'}->is_literal;
		
		$self->{definitions}++;
		push @{ $self->{'prefixes'} },
			[
				lc $row->{'prefix'}->literal_value,
				$row->{'uri'}->literal_value,
				1,
			];
	}

	# TODO - any terms or prefixes defined multiple times should
	# be ignored completely.
	
	my $vocab_results = $model->get_statements(
		undef,
		RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#vocabulary'),
		undef);
	while (my $st = $vocab_results->next)
	{
		next unless $st->object->is_literal;
		if (defined $self->{'vocab'})
		{
			$self->{definitions}--;
			$self->{'vocab'} = '';
			last;
		}
		else
		{
			$self->{definitions}++;
			$self->{'vocab'} = $st->object->literal_value;
		}
	}
	
	return $self;
}

sub _term_pattern
{
	my ($self) = @_;
	
	my $binding = RDF::Trine::Node::Variable->new('binding');
	
	return RDF::Trine::Pattern->new(
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#term'),
			RDF::Trine::Node::Variable->new('term'),
			),
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#uri'),
			RDF::Trine::Node::Variable->new('uri'),
			),
		);
}

sub _prefix_pattern
{
	my ($self) = @_;
	
	my $binding = RDF::Trine::Node::Variable->new('binding');
	
	return RDF::Trine::Pattern->new(
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#prefix'),
			RDF::Trine::Node::Variable->new('prefix'),
			),
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#uri'),
			RDF::Trine::Node::Variable->new('uri'),
			),
		);
}

sub get_terms
{
	my ($self) = @_;
	return @{ $self->{'terms'} };
}

sub get_prefixes
{
	my ($self) = @_;
	return @{ $self->{'prefixes'} };
}

sub get_vocabulary
{
	my ($self) = @_;
	return ($self->{'vocab'} eq '') ? undef : $self->{'vocab'};
}


1;

__END__

=head1 NAME

RDF::RDFa::Parser::Profile::RDF - profiles written in RDF

=head1 DESCRIPTION

This is the recommended format for profiles in RDFa 1.1.

This module provides C<new_from_response> and C<new_from_model>
constructors which may prove useful for subclassing.

=head1 SEE ALSO

L<RDF::RDFa::Parser>,
L<RDF::RDFa::Parser::Profile>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2008-2010 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
